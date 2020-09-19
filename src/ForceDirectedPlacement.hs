{-# LANGUAGE OverloadedLists,TemplateHaskell #-}
module ForceDirectedPlacement where

import Vector
import qualified Data.Vector as V
import Control.Lens
import System.Random
import qualified Graph as G
import qualified Graphics.Gloss as Gloss

-- グラフの力学的レイアウト導出アルゴリズム
data Vertex = Ver {_idVer :: Int,_position :: Vector , _disposition :: Vector } deriving(Show)

-- 頂点用レンズ
-- idVer = lens _idVer (\vx i -> vx{_idVer = i}) :: Lens' Vertex Int
-- position = lens _position (\vx pos -> vx{_position = pos}) :: Lens' Vertex Vector
-- disposition = lens _disposition (\vx disp -> vx{_disposition = disp}) :: Lens' Vertex Vector
$(makeLenses ''Vertex)



-- 射のcross積
cross f g = over _1 f . over _2 g

-- フレームの形状とグラフの頂点数から決定する定数
k_Cons :: Double -> Double -> [a] -> Double
k_Cons w h vs = let n = fromIntegral $ length vs in sqrt $ w * h / n


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
    -- と思ったが、Frameの初期値を生成する際にこの関数を使いたいのだった。やはり実数二つを引数に取る方が良い。
type NumberOfVertex = Int
type Width = Double
type Height = Double
mkInitCoor :: Width -> Height -> NumberOfVertex -> IO [(Double , Double)] -- 実際には幅、高さではなくFrameを渡すべきところだろう。あとで改良する。
mkInitCoor w h n = let trans x = x - 0.5 in map (cross ((*w) . trans) ((*h) . trans)) <$> mkRandomCoor n
    
-- 乱数座標を初期値にとったVertexの生成
mkVertex :: Width -> Height -> [Int] -> IO (V.Vector Vertex)
mkVertex w h ns = do
    let n = length ns
    coors <- mkInitCoor w h n
    let vs = map (uncurry V) coors
        protoVers = map (\n pos -> Ver n pos 0) ns
    return . V.fromList $ zipWith ($) protoVers vs

data Frame = Frame{_width :: Double,
                   _height :: Double, 
                   _temperature :: Double,
                   _vertices :: V.Vector Vertex,
                   _edges :: V.Vector (Int,Int),
                   _k_Constant :: Double,
                   _indexes :: V.Vector (Int,Int)
                   } deriving(Show)



$(makeLenses ''Frame)

type Temperature = Double

initFrame :: Width -> Height -> Temperature -> G.Graph Int -> IO Frame
initFrame w h t g = do 
    let (vs,es) = G.destruct $ g
        es' = fmap (cross (\x -> x - 1) (\x -> x - 1)) . V.fromList $ es
        k = sqrt $ (w*h) / (fromIntegral . length $ vs)
        ixs = V.fromList [(i-1,j-1 ) | i <- vs, j <- vs, i < j] -- Data.Vectorの実態は配列。インデックスは0から始まることに注意。(そのための-1)
    vers <- mkVertex w h vs
    return $ Frame w h t vers es' k ixs 

-- 2頂点間に働く力がもたらす、変位ベクトルの差分
repulsive__ :: Vertex -> Vertex -> Double -> Vector
repulsive__ (Ver n1 p1 d1) (Ver n2 p2 d2) k = if n1 == n2 then V 0 0 else
                    let delta = p2 - p1
                        z = norm delta
                        force = k*k / z -- kを定めていないことを忘れるべからず。Frameデータ型を定義したら引数にFrameを持たせるか？ 
                    in (force / z) *: delta

repulsive_ :: V.Vector Vertex -> Double -> (Int,Int) -> V.Vector Vertex
repulsive_ vs k (i,j) =
    let vi = vs V.! i
        vj = vs V.! j
        disp_delta = repulsive__ vi vj k
        disp_i = disp_delta
        disp_j = - disp_delta
    in V.accum (\x y -> over disposition (y+) x) vs [(i,disp_i),(j,disp_j)]

repulsive :: Frame -> Frame
repulsive frame = 
    let vs  = view vertices frame
        k   = view k_Constant frame
        idx = view indexes frame
        vs' = V.foldr (\ij v -> repulsive_ v k ij) vs idx
    in set vertices vs' frame



f_a d k = d*d/k

attractive_ :: V.Vector Vertex -> Double -> (Int,Int) -> V.Vector Vertex
attractive_ vs k (i,j) = if i == j then vs else
            let vi = vs V.! i
                vj = vs V.! j
                delta = _position vi - _position vj
                distance = xcoor . abs $ delta
                force = f_a distance k
                disp_i =  - (force / distance) *: delta
                disp_j =  (force / distance) *: delta
            in V.accum (\x y -> set disposition y x) vs [(i,disp_i), (j,disp_j)]

-- 頂点間の引力を変異ベクトルに作用させる関数のラッピング版
    -- フレームを受け取り、フレームから必要な情報を取り出して引力作用を計算し、フレームを更新して返す
attractive :: Frame -> Frame
attractive frame = 
    let vs  = view vertices frame
        k   = view k_Constant frame
        ixs = view indexes frame
        vs' = foldr (\i v -> attractive_ v k i) vs ixs
    in set vertices vs' frame


-- 
temperatureAction__ :: Double -> Double -> Double -> Vertex -> Vertex
temperatureAction__ w h t v = 
    let vxy = view disposition v -- Vertexの変位ベクトル
        vxy_norm = norm disp   -- 変位ベクトルの大きさ
        protonewpos = view position v + (min vxy_norm t) *: vxy    -- フレーム壁がなかった場合の頂点の移動先。このあと壁の影響を加味する
        mknewpos_ l d = min (l/2) . max (-l / 2) $ d    -- 壁にぶつかった頂点はそこで停止する仕様
        newPos_x = mknewpos_ w (xcoor protonewpos)  -- vの座標ベクトルのx座標。変位による移動先か、またはフレームの内壁で止まっている
        newPos_y = mknewpos_ h (ycoor protonewpos)  -- vの座標ベクトルのy座標も同様。
    in set position (V newPos_x newPos_y) v -- 頂点の座標ベクトルを更新
    
-- 斥力、引力による速度変化がもたらす座標変化を計算したあと、フレームの温度と内壁の影響を加味して、Vertexに新しい座標を設定する関
temperatureAction_ :: Frame -> V.Vector Vertex
temperatureAction_ frame =
    let w = view width frame -- 横幅
        h = view height frame   -- 縦の高さ
        t = view temperature frame  -- フレーム内温度
        vs = view vertices frame
    in fmap (temperatureAction__ w h t) vs -- 変位ベクトルが位置座標に及ぼす影響と、フレーム内温度と壁の影響を併せて、次の時点での位置ベクトルを導出

-- Frame上の更新関数に持ち上げ
temperatureAction :: Frame -> Frame
temperatureAction frame =
    let vs = temperatureAction_ frame
    in set vertices vs frame

cool :: Double -> Frame -> Frame
cool n frame = 
    let t = n * n / (n+1)
    in set temperature t frame

oneloop :: Frame -> Frame
oneloop = cool 10 . temperatureAction . repulsive . attractive