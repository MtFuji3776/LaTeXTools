{-# LANGUAGE TypeFamilies #-}

module Graph where

data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)

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

proj1 :: Graph a -> Graph a
proj1 (Overlay g1 g2) = g1
proj1 g = g

proj2 :: Graph a -> Graph a
proj2 (Overlay g1 g2) = g2
proj2 g = g

destruct :: Graph a -> [Graph a]
destruct Empty = []
destruct (Vertex x) = [Vertex x]
destruct (Overlay g1 g2) = destruct g1 ++ destruct g2
destruct (Connect g1 g2) = [Connect g1 g2]

getVertices :: Graph a -> [Graph a]
getVertices = destruct . proj1

getEdges :: Graph a -> [Graph a]
getEdges = destruct . proj2

data Node = Node {name :: Int, xCoor :: Double, yCoor :: Double , nodeLabel :: String}

instance Show Node where
    show (Node n x y l) = "\\node (" ++ show n ++ ") at (" ++ show x ++ "," ++ show y ++ ") {$" ++ l ++ "$};\n"

data Option = Monic | Epic | Cover deriving(Show) --Showインスタンスとしては後で適切に定義し直す。

data Draw = Draw{id :: Int, options :: [Option], dom :: Int, cod :: Int, drawLabel :: String}

instance Show Draw where
    show (Draw i o d c dr) = show i ++ ":\\draw" ++ show o ++ " (" ++ show d ++ ") to (" ++ show c ++ ");\n"

planeNode :: Graph Int -> Node -- Partial Function
planeNode (Vertex a) = Node a 0 0 ""

planeDraw :: Graph Int -> Int -> Draw -- Partial Function
planeDraw (Connect (Vertex n) (Vertex m)) x = Draw x [] n m ""

mkDraws :: Graph Int -> [Draw] --Partial Function
mkDraws g = let xs = getEdges g in zipWith planeDraw xs [1,2..]

data Diagram = Dia [Node] [Draw] deriving (Show)

planeDiagram :: Graph Int -> Diagram
planeDiagram g = Dia (map planeNode . getVertices $ g) (concatMap mkDraws . getEdges $ g)

