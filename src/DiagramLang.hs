{-# LANGUAGE TemplateHaskell #-}
module DiagramLang where

import Graph
import TikZ
import Control.Lens

type IDDraw = Int
data Form = Product IDDraw IDDraw
          | Equalize  IDDraw
          | Pullback  IDDraw IDDraw
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

-- Drawよりも図式言語に特化させた前駆体型
data Morphism a = Mor{_idMor :: Int
                     ,_arropts :: [ArrowOption]
                     , _domain :: a 
                     , _codomain :: a
                     , _labMor :: AttachNode}deriving(Show) --Showインスタンスはあとでもうちょっと考える

data ArrowOption = Monic | Epic | Cover | Equalizer | XShift Double | YShift Double | XYShift Double Double | ButtCap | Stealth | Custom String 

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

instance Show a => Render (Morphism a) where
    render (Mor i os d c l) = let o = foldr (\x ys -> if ys == [] then show x else show x ++ "," ++ ys) mempty os
                              in "\\draw[" ++  o ++ "] " ++ show d ++ " to " ++ show l ++ show c ++ ";"


$(makeLenses ''Morphism)


-- TikZのdrawのオプションで矢印に関するものを研究し、TikZのOptionをデータ型で構成できたときには、
-- そちらに準拠したShowインスタンスにリファクタリングする予定。

data ArrowOptions = ArrOpt{_idD :: Int,
                           _opts :: [ArrowOption],
                           _attachLabel :: AttachNode}deriving(Show)

data DiagramLang = DiaLan{_vert :: Vertical,
                          _objs :: [Node],
                          _arrs :: [Draw Int],
                          _symbs :: [Symbol],
                          _arrOpts :: [ArrowOptions]}


newline = "\n"

-- display :: (Foldable t,Show a) => t a -> String
-- display = foldr (\x ys-> show x ++ ys) ""

instance Show DiagramLang where
    show d = show (_vert d) ++ newline ++ display (_objs d) ++  display (_arrs d) ++ show (_symbs d) ++ newline ++ show (_arrOpts d)


$(makeLenses ''ArrowOptions)
$(makeLenses ''DiagramLang)