module Main where

--import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import ForceDirectedPlacement
import Control.Lens
import qualified Data.Vector as Vec
import Vector
import Graph (path)


window :: Display
window = InWindow "Hello World" (windowWidth,windowHeight) (100,100)

windowWidth,windowHeight :: Num a => a
windowWidth  = 960
windowHeight = 720

-- シミュレーションの実装
boxWidth,boxHeight :: Float
boxWidth  = 50
boxHeight = 50

data BoxState = BoxState{
    _x :: Float --x座標の位置
,   _y :: Float --y座標の位置
,   _vx :: Float -- x方向の速度
,   _vy :: Float -- y方向の速度
}

initialBox :: BoxState
initialBox = BoxState 0 0 0 0

drawBox :: BoxState -> Picture
drawBox box = translate (_x box) (_y box) $ rectangleSolid boxWidth boxHeight

-- nextBox :: ViewPort -> Float -> BoxState -> BoxState
-- nextBox vp dt box = 
--     let -- 速度を考慮した次のステップでの位置を計算
--         x = _x box + _vx box * dt
--         y = _y box + _vy box * dt

--         -- 壁との当たり判定
--         isOverTop    = y > (windowHeight - boxHeight) / 2
--         isOverBottom = y < -(windowHeight - boxHeight) / 2
--         isOverRight  = x > (windowWidth - boxWidth) / 2
--         isOverLeft   = x < -(windowWidth - boxWidth) / 2

--         -- 壁と衝突していれば速度を反転
--         vx = if isOverRight || isOverLeft then (-_vx box) else (_vx box)
--         vy = if isOverTop || isOverBottom then (-_vy box) else (_vy box)

--     in BoxState x y vx vy

nextBox :: Float -> BoxState -> BoxState
nextBox dt box = 
    let -- 速度を考慮した次のステップでの位置を計算
        x = _x box + _vx box * dt
        y = _y box + _vy box * dt
    in box {_x = x,_y = y}

-- ===============================================================================

updateBox :: Event -> BoxState -> BoxState --イベントを処理する関数。EventKey以外のイベントは無視する
updateBox (EventKey key ks _ _) box = updateBoxWithKey key ks box
updateBox (EventMotion _) box = box
updateBox (EventResize _) box = box

up,down,right,left :: BoxState -> BoxState
up box    = box{_vy = _vy box + 100}
down box  = box{_vy = _vy box - 100}
right box = box {_vx = _vx box + 100}
left box  = box {_vx = _vx box - 100}

updateBoxWithKey :: Key -> KeyState -> BoxState -> BoxState
updateBoxWithKey (SpecialKey KeyUp) ks = if ks == Down then up else down
updateBoxWithKey (SpecialKey KeyDown) ks = if ks == Down then down else up
updateBoxWithKey (SpecialKey KeyRight) ks = if ks == Down then right else left
updateBoxWithKey (SpecialKey KeyLeft) ks = if ks == Down then left else right
updateBoxWithKey (Char 'w') ks = if ks == Down then up else down
updateBoxWithKey (Char 's') ks = if ks == Down then down else up
updateBoxWithKey (Char 'd') ks = if ks == Down then right else left
updateBoxWithKey (Char 'a') ks = if ks == Down then left else right
updateBoxWithKey _ _ = id

mkedgePaths :: Frame -> [Path]-- pathvecsの処理はLens使えばもっと簡潔にできるはず
mkedgePaths frame = 
    let vs = view vertices frame
        es = view ForceDirectedPlacement.edges frame
        doubleTofloat = fromRational . toRational :: Double -> Float
        pathvecs = fmap (\t -> let i1 = fst t; i2 = snd t; vex1 = vs Vec.! i1; vex2 = vs Vec.! i2 in map (view position) [vex1,vex2]) es -- 辺タプルの値をインデックスにしてvsからVertex値を取得し、そのposition成分を取り出す関数のmap
    in  map (map (cross doubleTofloat doubleTofloat . toTuple)) $ Vec.toList pathvecs    -- VectorリストをDoubleタプルリストに変換、さらにDoubleをFloatに変換

foldn :: (a -> a) -> a -> Int -> a
foldn f e 0 = e
foldn f e n = foldn f (f e) (n-1)

main :: IO ()
--main = display window red (translate (-150) (-10) . scale 0.5 0.5 $ text "Hello World")
--main = animate window white (\t -> if t <= 10  then circle (3*t) else circle (-3*t))
--main = simulate window white 24 initialBox drawBox nextBox
--main = play window white 24 initialBox drawBox updateBox nextBox
--main = foldn (>>= return . (oneloop  40)) (initFrame 100 100 10 (1*(2+3) + (2+3)*4 + 5 * (1+2+3))) 1000 >>= \f -> display FullScreen white $ translate (-150) (-10) . scale 10 10 $  mconcat . map line $ mkedgePaths f
main = do
    f1 <- initFrame 10000 10000 40 $ 1*2*3
    print f1
    simulate FullScreen white 60 f1 (scale 10 10 . mconcat . map line . mkedgePaths) (\_ -> oneloop)
