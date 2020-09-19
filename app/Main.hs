module Main where

--import Lib
import Graphics.Gloss



window :: Display
window = InWindow "Hello World" (640,480) (100,100)

main :: IO ()
main = display window cyan (translate (-150) (-10) . scale 0.5 0.5 $ text "Hello World")
