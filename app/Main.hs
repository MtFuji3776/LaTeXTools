module Main where

--import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


window :: Display
window = InWindow "Hello World" (windowWidth,windowHeight) (100,100)

windowWidth,windowHeight :: Num a => a
windowWidth  = 640
windowHeight = 480

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
initialBox = BoxState 0 0 150 150

drawBox :: BoxState -> Picture
drawBox box = translate (_x box) (_y box) $ rectangleSolid boxWidth boxHeight

nextBox :: ViewPort -> Float -> BoxState -> BoxState
nextBox vp dt box = 
    let -- 速度を考慮した次のステップでの位置を計算
        x = _x box + _vx box * dt
        y = _y box + _vy box * dt

        -- 壁との当たり判定
        isOverTop    = y > (windowHeight - boxHeight) / 2
        isOverBottom = y < -(windowHeight - boxHeight) / 2
        isOverRight  = x > (windowWidth - boxWidth) / 2
        isOverLeft   = x < -(windowWidth - boxWidth) / 2

        -- 壁と衝突していれば速度を反転
        vx = if isOverRight || isOverLeft then (-_vx box) else (_vx box)
        vy = if isOverTop || isOverBottom then (-_vy box) else (_vy box)

    in BoxState x y vx vy


main :: IO ()
--main = display window red (translate (-150) (-10) . scale 0.5 0.5 $ text "Hello World")
--main = animate window white (\t -> if t <= 10  then circle (3*t) else circle (-3*t))
main = simulate window white 24 initialBox drawBox nextBox
