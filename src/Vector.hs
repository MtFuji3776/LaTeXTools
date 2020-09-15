{-# LANGUAGE TemplateHaskell,OverloadedLists #-}
module Vector where

import Control.Lens(lens,_1,_2,over,Lens',makeLenses)
import qualified Data.Vector as V

data Vector = V{xcoor :: Double,ycoor :: Double}

instance Show Vector where
    show (V x y) = "<" ++ show x ++ "," ++ show y ++ ">"

instance Num Vector where
    fromInteger n = let x = fromInteger n in V x x
    V x1 y1 + V x2 y2 = V (x1 + x2) (y1 + y2)
    V x1 y1 * V x2 y2 = V (x1 * x2) (y1 * y2)
    abs (V x y) = let r = sqrt (x*x + y*y) in V r r
    negate (V x y) = V (negate x) (negate y)
    signum (V x y) = V (signum x) (signum y)

toTuple :: Vector -> (Double,Double)
toTuple v = (xcoor v,ycoor v)

(*:) :: Double -> Vector -> Vector -- スカラー作用
k *: v = let v1 = V k k in v1 * v

norm :: Vector -> Double -- 2ノルム
norm = xcoor . abs

unit :: Vector -> Vector -- 単位ベクトル
unit v = let k = 1 / (norm v) in k *: v

(.:) :: Vector -> Vector -> Double -- 内積
v1 .: v2 = let V x y = v1 * v2 in x + y

normal :: Vector -> Vector --法線ベクトル
normal v = let V x y = unit v in V (-y) x

separate :: Double -> Double -> Vector -> Vector -> Vector -- 位置ベクトルの内外分点
separate m n v1 v2 = let k = 1 / (m+n) in (k*n) *: v1 + (k*m) *: v2

relvector :: Vector -> Vector -> Vector -- 第二引数を始点とした相対ベクトル
relvector v1 v2 = v1 - v2

protprod :: Vector -> Vector -> Vector -> Double -> (Vector,Vector) -- Product,Pullback記号導出のための座標計算の下準備
protprod v v1 v2 k = let p x = k *: relvector x v        -- 第一引数が始点、それ以降が終点を表すベクトル
                   in  over _1 p . over _2 p $ (v1,v2)    -- 相対ベクトルを計算し、1/4サイズにして返す

pullback :: Vector -> Vector -> Vector -> [Vector] -- 引き戻し記号でPathが辿るべき頂点座標を、位置ベクトルのリストで返す関数
pullback v v1 v2 = let (v1',v2') = protprod v v1 v2 0.25 -- 記号の基点をprotprodで計算
                       vs = [v1',v1' + v2',v2'] -- v1' + v2'が中継点。Pullback固有でProductと違うところ
                   in map (+v) vs -- 相対ベクトルの補正を修正

product :: Vector -> Vector -> Vector -> [(Vector,Vector)]
product v v1 v2 = let (v1',v2') = protprod v v1 v2 0.25 -- 記号の始点
                      (v1'',v2'') = protprod v v1 v2 0.5 -- Pathの終点はPullbackと異なる方法で用意する
                      vs = [(v1',v1' + v2''),(v2' + v1'' ,v2')] -- Product記号の描画。これはPathではなくDrawで描くべし。
                  in map (over _1 (+v).over _2 (+v)) vs




-- グラフの力学的レイアウト導出アルゴリズム
data Vertex = Ver {_idVer :: Int,_position :: Vector , _disposition :: Vector } deriving(Show)

-- 頂点用レンズ
-- idVer = lens _idVer (\vx i -> vx{_idVer = i}) :: Lens' Vertex Int
-- position = lens _position (\vx pos -> vx{_position = pos}) :: Lens' Vertex Vector
-- disposition = lens _disposition (\vx disp -> vx{_disposition = disp}) :: Lens' Vertex Vector
$(makeLenses ''Vertex)

k w h vs = let n = fromIntegral $ length vs in sqrt $ w * h / n

repulsive (Ver n1 p1 d1) (Ver n2 p2 d2) k = if n1 == n2 then V 0 0 else
                    let delta = p2 - p1
                        z = xcoor . abs $ delta
                        force = k*k / z -- kを定めていないことを忘れるべからず。Frameデータ型を定義したら引数にFrameを持たせるか？ 
                    in (force / z) *: delta

f_a d k = d*d/k

attractive :: (Int,Int) -> V.Vector Vertex -> Double -> V.Vector Vertex
attractive (i,j) vs k = if i == j then vs else
            let vi = vs V.! i
                vj = vs V.! j
                delta = _position vi - _position vj
                distance = xcoor . abs $ delta
                force = f_a distance k
                disp_i = _disposition vi - (force / distance) *: delta
                disp_j = _disposition vj + (force / distance) *: delta
            in V.accum (\x y -> over disposition (y+) x) vs [(i,disp_i), (j,disp_j)]

