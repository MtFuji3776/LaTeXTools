module DiagramLang where

import Graph
import TikZ

data Symbols a = Product a
               | Equalize a
               | Pullback a
               | CoProduct a
               | CoEqualizer a
               | Pushout a
               | Reticle a deriving(Show)

data Quantifier = EmptyQ | Forall | Exists | ExistsOnly deriving(Eq)

instance Show Quantifier where
    show EmptyQ = ""
    show Forall = "$\\forall$"
    show Exists = "$\\exists$"
    show ExistsOnly = "$\\exists !$"

data Vertical = Eps | Vertical Double Double Quantifier

instance Show Vertical where
    show (Vertical y1 y2 q) = "&\n\\draw[-Butt Cap] (0," ++ show (y1 - 0.2)
                                                         ++ ") to (0,"
                                                         ++ show (y2 + 0.2)
                                                         ++ ") node[above] {"
                                                         ++ show q
                                                         ++ "};\n&\n"

type Label = String
data AttachNode = ANode Place Label

data Place = Above 
           | Below 
           | LeftN 
           | RightN 
           | AboveRight 
           | AboveLeft 
           | BelowLeft 
           | BelowRight
instance Show Place where
    show Above      = "above"
    show Below      = "below"
    show LeftN       = "left"
    show RightN      = "right"
    show AboveRight = "above right"
    show AboveLeft  = "above left"
    show BelowLeft  = "below left"
    show BelowRight = "below right"

data ArrowOption = Monic | Epic | Cover | Equalizer | XShift Double | YShift Double | XYShift Double Double | ButtCap | Stealth | Custom String 

-- TikZのdrawのオプションで矢印に関するものを研究し、TikZのOptionをデータ型で構成できたときには、
-- そちらに準拠したShowインスタンスにリファクタリングする予定。
instance Show ArrowOption where
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

data ArrowOptions = ArrOpt{idD :: Int,
                           opts :: [ArrowOption],
                           attachLabel :: AttachNode}deriving(Show)

data DiagramLang = DiaLan{vert :: Vertical,
                          objs :: [Node],
                          arrs :: [Draw],
                          symbs :: [Symbols],
                          arrOpts :: [ArrowOptions]}deriving(Show)

