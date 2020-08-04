{-# LANGUAGE TypeFamilies #-}

module Graph where

import Data.Set(Set,empty,singleton,union,fromList,toList)
import Data.List(sort)
import qualified Data.Graph as G

data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)

instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance Show a => Show (Graph a) where
    show Empty = "()"
    show (Vertex a) = show a
    show (Overlay x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Connect x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

edge :: a -> a -> Graph a
edge x y = Connect (Vertex x) (Vertex y)

vertices :: [a] -> Graph a
vertices = foldr Overlay Empty . map Vertex

clique :: [a] -> Graph a
clique = foldr Connect Empty .map Vertex

edges :: [(a,a)] -> Graph a
edges = foldr Overlay Empty . map (uncurry edge)

graph' :: [a] -> [(a,a)] -> Graph a
graph' vs es = Overlay (vertices vs) (edges es)

graph :: ([a],[(a,a)]) -> Graph a
graph = uncurry graph'

normalize :: Ord a => Graph a -> Graph a
normalize = graph . destruct

buildG :: Num a => [a] -> [Graph a] -> Graph a -- 第二引数はVertex a2 * Vertex a2の形のグラフのみ書くこと。…GADTでConnectに固有の型をつけたほうが確実か？
buildG xs es = vertices xs + foldr (+) Empty es

proj1 :: Graph a -> Graph a
proj1 (Overlay g1 g2) = g1
proj1 g = g

proj2 :: Graph a -> Graph a
proj2 (Overlay g1 g2) = g2
proj2 g = g

-- destruct :: Graph a -> [Graph a] -- 点集合と辺集合を同時に適切に得る方法が思いつかない。
-- destruct Empty = []
-- destruct (Vertex x) = [Vertex x]
-- destruct (Overlay g1 g2) = destruct g1 ++ destruct g2
-- destruct (Connect g1 g2) = destruct g1 ++ destruct g2 ++ [Connect x y | x <- destruct g1, y<- destruct g2]


getVertices :: Ord a => Graph a -> Set a
getVertices Empty = empty
getVertices (Vertex x) = singleton x
getVertices (Overlay g1 g2) = union (getVertices g1) (getVertices g2)
getVertices (Connect g1 g2) = union (getVertices g1) (getVertices g2)

getEdges :: Ord a => Graph a -> [(a,a)]
getEdges Empty = []
getEdges (Vertex x) = []
getEdges (Overlay g1 g2) = getEdges g1 ++ getEdges g2
getEdges (Connect g1 g2) = getEdges g1 ++ getEdges g2 ++ [(x,y) | x <- toList(getVertices g1), y <- toList (getVertices g2)]

destruct :: Ord a => Graph a -> ([a],[(a,a)]) --頂点集合と辺集合それぞれの構成で二回再帰を回さなくちゃいけないのがちょっと気持ち悪い。一回で済ませる方法はないか？
destruct g = (toList . getVertices $ g,sort . getEdges $ g)
-- 一回で済ませる方法は一応ある。蓄積引数法またはStateモナドを使うのだ。それでタプルを状態に持ち、第一成分ではgetVerticesの関数を、第二成分ではgetEdgesを計算させる。
-- このやり方なら、引数としてgを一つ受け取ればよく、現状のdeconstractのようにgの再帰構造を二回壊すよりも半分の時間で済むはずだ。

-- uncurry graph . deconstract = id
-- deconstract . uncurry graph = id
-- が成立。コンストラクタとデストラクタで同型。

data Rose a = Rose a [Rose a] deriving(Show)

instance Functor Rose where
    fmap f (Rose x rs) = Rose (f x) $ map (fmap f) rs

--この定義は問題なく動作するが、filterとmapをあまりに繰り返し過ぎている気がしなくもない。タプルのリストをArrayにすることで計算時間を優位に改善できる気がしている。
generate :: Ord a => Graph a -> a -> Rose a
generate g v = let ts = snd (destruct g)
                   children = map snd . filter (\t -> fst t == v) $ ts -- 第一成分がvになるタプルの第二成分全体のリスト＝v直下の部分木の根
               in Rose v $ map (generate g) children


-- ここから図式言語
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

setup :: ([Int],[(Int,Int)]) -> ([Graph Int],[Graph Int])
setup = cross (map Vertex) (map (uncurry Connect . cross Vertex Vertex))

cross :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
cross f g (x,y) = (f x,g y)

planeNode :: Int -> Node
planeNode a = Node a 0 0 ""

planeDraw :: (Int,Int) -> Int -> Draw Int
planeDraw (n,m) x = Draw x [] n m ""

nodedraws :: ([Int],[(Int,Int)]) -> ([Node],[Draw Int])
nodedraws (ns,es) = let es' = zip es [1..] in cross (map planeNode) (map (uncurry planeDraw)) (ns,es')

-- Draw値の更新オペレータ
setOption :: [Option] -> Draw a -> Draw a
setOption os (Draw n ops d c l) = Draw n os d c l

setLabel :: String -> Draw a -> Draw a
setLabel l (Draw n os d c l') = Draw n os d c l

-- プレーンなDrawのOptionとLabelを更新するオペレータ。setOptLab'はID
setOptLab' n op l d = if n == idDraw d then setOption op . setLabel l $ d
                                       else d

setOptLab op l d = setOptLab' (idDraw d) op l d

view :: Show a => [a] -> IO ()
view = putStrLn . concatMap show

-- mkDraws :: Graph Int -> [Draw] --Partial Function
-- mkDraws g = let xs = getEdges g in zipWith planeDraw xs [1,2..]

data Diagram a = Dia [Node] [Draw a] deriving (Show)

-- planeDiagram :: Graph Int -> Diagram
-- planeDiagram g = Dia (map planeNode . getVertices $ g) (concatMap mkDraws . getEdges $ g)

