module TikZ where

import Graph


data Vector = V Double Double deriving(Eq,Ord,Show)

instance Num Vector where
    fromInteger n = let m = fromInteger n in V m m
    (V x y) + (V z w) = V (x+z) (y+w)
    (V x y) * (V z w) = V (x*z) (y*w)
    abs (V x y) = V (sqrt $ x*x + y*y) 0
    negate (V x y) = V (-x) (-y)
    signum (V x y) = V (signum x) (signum y)

-- ここから図式言語
data Node = Node {name :: Int, xCoor :: Double, yCoor :: Double , nodeLabel :: String} deriving(Eq)

instance Ord Node where
    n1 <= n2 = name n1 <= name n2

instance Show Node where
    show (Node n x y l) = "\\node (" ++ show n ++ ") at (" ++ show x ++ "," ++ show y ++ ") {$" ++ l ++ "$};\n"

setCoor :: Double -> Double -> Node -> Node
setCoor x y (Node n z w l) = Node n x y l

setLabelN :: String -> Node -> Node
setLabelN l (Node n x y l') = Node n x y l

setNode :: Double -> Double -> String -> Node -> Node
setNode x y l = setLabelN l . setCoor x y

instance Num Node where
    fromInteger n = let m = fromInteger n in Node m 0 0 ""
    (Node n1 x y l) + (Node n2 z w l') = Node n1 (x+z) (y+w) l
    (Node n1 x y l) * (Node n2 z w l') = Node n1 (x*z) (y*w) l
    abs (Node n x y l) = Node n (sqrt $ x*x + y*y) 0 l
    negate (Node n x y l) = Node n (-x) (-y) l
    signum (Node n x y l) = Node n (signum x) (signum y) l


fromVector :: Vector -> Node
fromVector (V x y) = Node 0 x y ""

toVector :: Node -> Vector
toVector (Node n x y l) = V x y

(+:) :: Node -> Vector -> Node
n +: v = n + fromVector v


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

