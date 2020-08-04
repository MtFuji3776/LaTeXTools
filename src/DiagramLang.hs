module DiagramLang where

import Graph

data Node = Node {name :: Int, xCoor :: Double, yCoor :: Double , nodeLabel :: String}

instance Show Node where
    show (Node n x y l) = "\\node (" ++ show n ++ ") at (" ++ show x ++ "," ++ show y ++ ") {$" ++ l ++ "$};\n"

data Option = Monic | Epic | Cover | Equalizer | XShift Double | YShift Double | XYShift Double Double | ButtCap | Stealth | Custom String 

instance Show Option where
    show Monic = "|-stealth"
    show Cover = "-{Stealth[open]}"
    show Epic  = "->{stealth}"
    show Equalizer = ">-stealth"
    show (XShift x) = "transform canvas = {xshift = " ++ show x ++ "}"
    show (YShift y) = "transform canvas = {yshift = " ++ show y ++ "}"
    show (XYShift x y) = "transform canvas = {xshift = " ++ show x ++ ",yshift = " ++ show y ++ "}"
    show ButtCap = "-Butt Cap"
    show Stealth = "-stealth"
    show (Custom xs) = xs

data Draw a = Draw{idDraw :: Int, options :: [Option], dom :: a, cod :: a, drawLabel :: String}

instance Show a => Show (Draw a) where
    show (Draw i o d c dr) = show i ++ ":\\draw" ++ show o ++ " (" ++ show d ++ ") to (" ++ show c ++ ");\n"

instance Functor Draw where
    fmap f (Draw n os d c l) = Draw n os (f d) (f c) l




type Label = String
data AttachNode = ANode Place Label

data Place = Above 
           | Below 
           | Left 
           | Right 
           | AboveRight 
           | AboveLeft 
           | BelowLeft 
           | BelowRight
instance Show Place where
    show Above      = "above"
    show Below      = "below"
    show Left       = "left"
    show Right      = "right"
    show AboveRight = "above right"
    show AboveLeft  = "above left"
    show BelowLeft  = "below left"
    show BelowRight = "below right"

