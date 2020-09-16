module ForceDirectedPlacement where

import Vector
import qualified Data.Vector as V
import Control.Lens

-- グラフの力学的レイアウト導出アルゴリズム
data Vertex = Ver {_idVer :: Int,_position :: Vector , _disposition :: Vector } deriving(Show)

-- 頂点用レンズ
-- idVer = lens _idVer (\vx i -> vx{_idVer = i}) :: Lens' Vertex Int
-- position = lens _position (\vx pos -> vx{_position = pos}) :: Lens' Vertex Vector
-- disposition = lens _disposition (\vx disp -> vx{_disposition = disp}) :: Lens' Vertex Vector
$(makeLenses ''Vertex)

-- 射のcross積
cross f g = over _1 f . over _2 g

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

-- 1~wのランダム選択する関数。引数wで分母の数を決める。wが100ならば1/100 , 2/100 , ... , 100/100の中からランダム選択される。
uniform0_n :: (Random a, Num a) => a -> IO a
uniform0_n w = getStdRandom (randomR (1,w)) 

-- 1~10億の中からランダム選択。
uniform0_billion :: IO Int
uniform0_billion = uniform0_n 1000000000

-- 区間[0,1]上の有理数集合のうち{n/1000000000 | n <- [0..1000000000]}からランダム選択。
uniform0_1 :: (Fractional b1,Fractional b2) => IO (b1,b2)
uniform0_1 = do
    d1 <- uniform0_billion
    d2 <- uniform0_billion
    let d1' = fromIntegral d1 / 1000000000
        d2' = fromIntegral d2 / 1000000000
    return (d1' , d2')


-- 閉領域[0,1]×[0,1]からn組の点をランダム選択する関数
mkRandomCoor ::  Int -> IO [(Double,Double)]
mkRandomCoor n = mapM id (replicate n uniform0_1) 

-- フレームの横幅wと縦幅hを渡すと、閉領域[0,w]×[0,h]からn点ランダム選択する関数。実際に使うのはこれでよかろう。
mkInitCoor :: Frame -> Int -> IO [(Double , Double)] -- 実際には幅、高さではなくFrameを渡すべきところだろう。あとで改良する。
mkInitCoor frame n = do
    ts <- mkRandomCoor n
    let w = _width frame
        h = _height frame
    return $ map (cross (*w) (*h)) ts

data Frame = Frame{_width :: Double,_height :: Double, temperature :: Double} deriving(Show)

$(makeLenses ''Frame)
