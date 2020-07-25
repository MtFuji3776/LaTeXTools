{-# LANGUAGE TypeFamilies #-}

module Graph where

import Data.Set(Set,empty,singleton,union,fromList,toList)
import Data.List(sort)

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

graph :: [a] -> [(a,a)] -> Graph a
graph vs es = Overlay (vertices vs) (edges es)

buildG :: Num a => [a] -> [Graph a] -> Graph a -- 第二引数はVertex a2 * Vertex a2の形のグラフのみ書くこと。…GADTでConnectに固有の型をつけたほうが確実か？
buildG xs es = vertices xs + foldr (+) Empty es

proj1 :: Graph a -> Graph a
proj1 (Overlay g1 g2) = g1
proj1 g = g

proj2 :: Graph a -> Graph a
proj2 (Overlay g1 g2) = g2
proj2 g = g

destruct :: Graph a -> [Graph a] -- 点集合と辺集合を同時に適切に得る方法が思いつかない。
destruct Empty = []
destruct (Vertex x) = [Vertex x]
destruct (Overlay g1 g2) = destruct g1 ++ destruct g2
destruct (Connect g1 g2) = destruct g1 ++ destruct g2 ++ [Connect x y | x <- destruct g1, y<- destruct g2]


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

deconstract :: Ord a => Graph a -> ([a],[(a,a)]) --頂点集合と辺集合それぞれの構成で二回再帰を回さなくちゃいけないのがちょっと気持ち悪い。一回で済ませる方法はないか？
deconstract g = (toList . getVertices $ g,sort . getEdges $ g)

-- uncurry graph . deconstract = id
-- deconstract . uncurry graph = id
-- が成立。コンストラクタとデストラクタで同型。

data Node = Node {name :: Int, xCoor :: Double, yCoor :: Double , nodeLabel :: String}

instance Show Node where
    show (Node n x y l) = "\\node (" ++ show n ++ ") at (" ++ show x ++ "," ++ show y ++ ") {$" ++ l ++ "$};\n"

data Option = Monic | Epic | Cover | Equalizer | XShift Double | YShift Double | XYShift Double Double | ButtCap | Stealth | Custom String 

instance Show Option where
    show Monic = "[|-stealth]"
    show Cover = ""

data Draw = Draw{idDraw :: Int, options :: [Option], dom :: Int, cod :: Int, drawLabel :: String}

instance Show Draw where
    show (Draw i o d c dr) = show i ++ ":\\draw" ++ show o ++ " (" ++ show d ++ ") to (" ++ show c ++ ");\n"

planeNode :: Graph Int -> Node -- Partial Function
planeNode (Vertex a) = Node a 0 0 ""

planeDraw :: Graph Int -> Int -> Draw -- Partial Function
planeDraw (Connect (Vertex n) (Vertex m)) x = Draw x [] n m ""

-- mkDraws :: Graph Int -> [Draw] --Partial Function
-- mkDraws g = let xs = getEdges g in zipWith planeDraw xs [1,2..]

data Diagram = Dia [Node] [Draw] deriving (Show)

-- planeDiagram :: Graph Int -> Diagram
-- planeDiagram g = Dia (map planeNode . getVertices $ g) (concatMap mkDraws . getEdges $ g)

