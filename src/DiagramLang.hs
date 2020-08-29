module DiagramLang where

import Graph
import TikZ

data Form = Product  
          | Equalize  
          | Pullback  
          | CoProduct  
          | CoEqualizer  
          | Pushout  
          | Reticle deriving(Show)

data Symbol = Sym Form Int (Graph Int) deriving(Show)





data Quantifier = EmptyQ | Forall | Exists | ExistsOnly deriving(Eq)

instance Show Quantifier where
    show EmptyQ = ""
    show Forall = "$\\forall$"
    show Exists = "$\\exists$"
    show ExistsOnly = "$\\exists !$"

data Vertical = NoVertical | Vertical Double Double Quantifier

-- Draw a型のShowインスタンスを真面目に作ることでここら辺はDraw aに依存できる。
-- これまでに作ったDraw a型は、Draw構文のうちto文法に属するものと解釈できる。TikZマニュアルでDrawの文法をちゃんと分析して、各文法に対応したデータ型を作るとここら辺もスッキリするはず。
-- 基本となるDraw型をしっかり作ることで、Morph型もVertical型などDrawを主体に使うデータは全てDraw a型に帰着させられるはずだ。
instance Show Vertical where
    show NoVertical = ""
    show (Vertical y1 y2 q) = "&\n\\draw[-Butt Cap] (0," ++ show (y1 - 0.2)
                                                         ++ ") to (0,"
                                                         ++ show (y2 + 0.2)
                                                         ++ ") node[above] {"
                                                         ++ show q
                                                         ++ "};\n&\n"

-- type Label = String
-- data AttachNode = ANode Place Label deriving(Show)

-- data Place = EpsPlace
--            | Above 
--            | Below 
--            | LeftN 
--            | RightN 
--            | AboveRight 
--            | AboveLeft 
--            | BelowLeft 
--            | BelowRight
-- instance Show Place where
--     show EpsPlace   = ""
--     show Above      = "above"
--     show Below      = "below"
--     show LeftN       = "left"
--     show RightN      = "right"
--     show AboveRight = "above right"
--     show AboveLeft  = "above left"
--     show BelowLeft  = "below left"
--     show BelowRight = "below right"

-- -- AttachNodeをDrawに作用させるオペレータ
-- (/\) :: Draw a -> Label -> Draw a
-- (Draw n ops d c )

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
                          arrs :: [Draw Int],
                          symbs :: [Symbol],
                          arrOpts :: [ArrowOptions]}

newline = "\n"

-- display :: (Foldable t,Show a) => t a -> String
-- display = foldr (\x ys-> show x ++ ys) ""

instance Show DiagramLang where
    show d = show (vert d) ++ newline ++ display (objs d) ++  display (arrs d) ++ show (symbs d) ++ newline ++ show (arrOpts d)

