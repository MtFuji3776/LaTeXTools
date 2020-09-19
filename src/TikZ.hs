{-# LANGUAGE MultiParamTypeClasses #-}
module TikZ where

import qualified Data.Map as Map
import Graph
import Data.Semigroup(Semigroup)

-- 座標計算用にベクトル型を定義しておく。データ構造のData.Vectorと違い数値計算に特化させる。
-- 事によると、Vector単体でモジュールを作っても良いかもしれない。
data TwoDimVector f = V{fstDim :: f, sndDim :: f }deriving(Eq,Ord)
type Vector = TwoDimVector Double

instance Show f => Show (TwoDimVector f) where
    show (V x y) = show x ++ " , " ++ show y
-- Num型クラスのインスタンス化。成分ごとの積を定義して、まずは可換環として扱う。スカラー倍は乗法の制限として定義できる。
-- どうせならばVectorSpace型クラスでも作るか？
instance Floating f =>  Num (TwoDimVector f) where
    fromInteger n = let m = fromInteger n in V m m
    (V x y) + (V z w) = V (x+z) (y+w)
    (V x y) * (V z w) = V (x*z) (y*w)
    abs (V x y) = V (sqrt $ x*x + y*y) 0
    negate (V x y) = V (-x) (-y)
    signum (V x y) = V (signum x) (signum y)

-- class VectorSpace v where
--     fromField :: f -> v
--     (+|) :: v -> v -> v
--     (*|) :: f -> v -> v


-- instance Floating f => VectorSpace (TwoDimVector f) where
--     fromField k = V k k
--     (+|) = (+)
--     (*|) k v = fromField k * v 

(*|) k v = V k k * v

-- 法線ベクトル
normalVector :: Vector -> Vector
normalVector (V 0 0) = V 0 0
normalVector (V x y) = let k = 1 / (sqrt $ x*x + y*y) in k *| V (-y) x

-- 内積
innerProduct :: Vector -> Vector -> Double
innerProduct v1 v2 = let V x y = v1 * v2 in x + y

-- m:nの内分点または外分点を求める。
innerSeparate :: Vector -> Vector -> Double -> Double -> Vector
innerSeparate v1 v2 n1 n2 = let n = 1/(n1+n2) in (n2*n) *| v1 + (n1*n) *| v2

deducePullback :: Vector -> Vector -> Vector -> [Vector]
deducePullback v1 v2 v3 = let v = innerSeparate v1 v2 3 1; v' = innerSeparate v3 v2 3 1 in [v, v+ (1/4)*|(v3 - v2),v']

vectorToDraw :: Vector -> Vector -> Draw Vector
vectorToDraw v1 v2 = Draw 0 [] v1 v2 NoLabel

-- ここから図式言語
data Node = Node {name :: Int, xCoor :: Double, yCoor :: Double , nodeLabel :: String} deriving(Eq)

instance Ord Node where
    n1 < n2 = name n1 < name n2
    n1 <= n2 = name n1 <= name n2

instance Show Node where
    show (Node n x y l) = "\\node (" ++ show n ++ ") at (" ++ show x ++ "," ++ show y ++ ") {$" ++ l ++ "$};\n"


data ProtoNode = PN Double Double String deriving(Show)

-- 単位付き左作用型クラス。これを用いて、グラフが生成したNodeに対して最小限の入力で初期化する。
-- と思ったらなんか左単位元作れないっぽい。両方の型引数を使った演算子か認めないのか？
class Action k v where 
    (<+>)   :: k -> v -> v

class MainKey a where
    itsKey :: a -> Int

consNodes :: Graph Double -> [Node]
consNodes = let setName n (Node n' x y l) = Node n x y l
                coor (x,y) = Node 0 x y ""
             in zipWith setName [1..] . map coor . snd . destruct

setNodeLabel :: [Node] -> [String] -> [Node]
setNodeLabel = let setNLabel (Node n x y l') l = Node n x y l
               in zipWith setNLabel

setNodes :: [String] -> Graph Double -> [Node]
setNodes ls = flip setNodeLabel ls . consNodes

instance Action ProtoNode Node where
    (PN x y l) <+> (Node n z w m) = Node n x y l

-- fromPN :: ProtoNode -> Node
-- fromPN (PN x y l) = Node 0 x y l

-- actNode :: ProtoNode -> Node -> Node
-- actNode pn n = fromPN pn + n

-- setCoor :: Double -> Double -> Node -> Node
-- setCoor x y (Node n z w l) = Node n x y l

-- setLabelN :: String -> Node -> Node
-- setLabelN l (Node n x y l') = Node n x y l

-- setNode :: Double -> Double -> String -> Node -> Node
-- setNode x y l = setLabelN l . setCoor x y

instance Num Node where
    fromInteger n = let m = fromInteger n in Node m 0 0 ""
    (Node n1 x y l) + (Node n2 z w l') = Node n2 (x+0) (y+0) l
    (Node n1 x y l) * (Node n2 z w l') = Node n2 (x*z) (y*w) l
    abs (Node n x y l) = Node n (sqrt $ x*x + y*y) 0 l
    negate (Node n x y l) = Node n (-x) (-y) l
    signum (Node n x y l) = Node n (signum x) (signum y) l

instance MainKey Node where
    itsKey = name

-- instance Semigroup Node where
--     mappend = (+)

instance Semigroup Node where
    n1 <> n2 = n1 + n2

instance Monoid Node where
    mempty = Node 0 0 0 ""
    mappend = (<>)

fromVector :: Vector -> Node
fromVector (V x y) = Node 0 x y ""

toVector :: Node -> Vector
toVector (Node n x y l) = V x y

(+:) :: Node -> Vector -> Node
n +: v = n + fromVector v

-- 以下、Node,Drawの連想配列を構成するためのコンビネータ

-- [Node]を連想配列で扱うためのコンビネータ
diagonal :: a -> (a,a)
diagonal x = (x,x)

swap :: (a -> b -> c) -> b -> a -> c
swap f x y = f y x

-- 第1成分に関数を作用させる関数。fmapの類縁。タプルをswapしてfmapしてまたswapするのと同じ。
actionFst :: (a -> b) -> (a,a1) -> (b,a1)
actionFst f = cross f id

-- Node,Drawを渡すと、そのName/IDとペアにする関数。連想配列をfromListで行うためのツール。
bindKey :: (a -> b) -> a -> (b,a)
bindKey f = actionFst f . diagonal

nameAndNode :: Node -> (Int,Node)
nameAndNode = bindKey name

idAndDraw :: Draw a -> (Int,Draw a)
idAndDraw = bindKey idDraw

-- Node,Drawのリストの組を、その主キーとの組に作り変える
nodedrawWithKey :: ([Node],[Draw a]) -> ([(Int,Node)],[(Int,Draw a)])
nodedrawWithKey = cross (map nameAndNode) (map idAndDraw)

-- 主キーとADT値の組を連想配列に変換
nodedrawMaps :: ([(Int,Node)],[(Int,Draw a)]) -> (Map.Map Int Node,Map.Map Int (Draw a))
nodedrawMaps = cross Map.fromList Map.fromList

newtype NodeDraws = ND{unND :: (NodeMap,DrawMap Int)} deriving(Show)

-- グラフデータから連想配列の組まで一気に変換する関数
generateNodeDraws :: Graph Int -> NodeDraws
generateNodeDraws = ND . nodedrawMaps . nodedrawWithKey . nodedraws . destruct

appendToMap :: (Num a, MainKey a) => [a] -> Map.Map Int a -> Map.Map Int a
appendToMap xs m = foldr (\x ys -> let z = head xs; n = itsKey z in Map.adjust (z+) n ys) m xs

mkMap :: MainKey a => [a] -> Map.Map Int a
mkMap xs = Map.fromList $ zip (map itsKey xs) xs

-- instance Action [a] NodeDraws where
--     ns <+> ms = let mapNode = fst (unND ms); draws = snd (unND ms) in ND (appendToMap ns mapNode, draws)

-- Nodeの初期値リストを渡すと、連想配列のそれを更新する関数
appendNodes :: [Node] -> Map.Map Int Node -> Map.Map Int Node
appendNodes = appendToMap
-- appendNodes []        = id
-- appendNodes (n:ns)    = appendNodes ns . Map.adjust (n+) (name n)

appendProtoDraws :: Num a => [ProtoDraw] -> DrawMap a -> DrawMap a
appendProtoDraws []     = id
appendProtoDraws (pd:pds) = appendProtoDraws pds . Map.adjust (actDraw pd) (idPD pd)

type NodeMap = Map.Map Int Node
type DrawMap a = Map.Map Int (Draw a)
type NodeDrawMaps a = (NodeMap, DrawMap a)

arrangeNodes :: [Node] -> (NodeMap,DrawMap a) -> (NodeMap,DrawMap a)
arrangeNodes ns = cross (appendNodes ns) id

arrangeDraws :: Num a => [ProtoDraw] -> (NodeMap,DrawMap a) -> NodeDrawMaps a
arrangeDraws pds = cross id (appendProtoDraws pds)

arrangeNodeDraws :: Num a => [Node] -> [ProtoDraw] -> NodeDrawMaps a -> NodeDrawMaps a
arrangeNodeDraws ns pds = cross (appendNodes ns) (appendProtoDraws pds)

display :: (Foldable t,Show a) => t a -> String
display = foldr (\x ys-> show x ++ ys) ""

-- Name = Intで紐付けされた関数をNodeに適用するための高階関数
evalByName :: (Int -> Node -> t) -> Node -> t
evalByName f n = let m = name n in f m n

--data Option = Moni deriving(Show)-- | Epic | Cover | Equalizer | XShift Double | YShift Double | XYShift Double Double | ButtCap | Stealth | Custom String 

-- instance Show Option where
--     show Monic = "|-stealth"
--     show Cover = "-{Stealth[open]}"
--     show Epic  = "->{stealth}"
--     show Equalizer = ">-stealth"
--     show (XShift x) = "transform canvas = {xshift = " ++ show x ++ "}"
--     show (YShift y) = "transform canvas = {yshift = " ++ show y ++ "}"
--     show (XYShift x y) = "transform canvas = {xshift = " ++ show x ++ ",yshift = " ++ show y ++ "}"
--     show ButtCap = "-Butt Cap"
--     show Stealth = "-stealth"
--     show (Custom xs) = xs

type Label = String
data AttachNode = NoLabel | ANode Place Label

instance Show AttachNode where
    show NoLabel    = ""
    show (ANode p l) = "node[" ++ show p ++ "] {$" ++ l ++ "$}"

data Place = EpsPlace
           | Above 
           | Below 
           | LeftN 
           | RightN 
           | AboveRight 
           | AboveLeft 
           | BelowLeft 
           | BelowRight
instance Show Place where
    show EpsPlace   = ""
    show Above      = "above"
    show Below      = "below"
    show LeftN       = "left"
    show RightN      = "right"
    show AboveRight = "above right"
    show AboveLeft  = "above left"
    show BelowLeft  = "below left"
    show BelowRight = "below right"

-- AttachNodeをDrawに作用させるオペレータ群
-- Aboveをつけるオペレータ。不細工な上矢印記号と思え。
(/|\) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) /|\ s = PD n ops $ ANode Above s

-- AboveRightをつけるオペレータ。中心から見て右上を表す記号
(|\) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) |\ s = PD n ops $ ANode AboveRight s

-- RightN.右を表す。
(|>) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) |> s = PD n ops $ ANode RightN s

--
(|/) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) |/ s = PD n ops $ ANode BelowRight s

(\|/) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) \|/ s = PD n ops $ ANode Below s

(\|) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) \| s = PD n ops $ ANode BelowLeft s

(<|) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) <| s = PD n ops $ ANode LeftN s

(/|) :: ProtoDraw -> Label -> ProtoDraw
(PD n ops l) /| s = PD n ops $ ANode AboveLeft s


class Show a => Render a where
    render :: a -> String

type Option = String
data Draw a = Draw{idDraw :: Int, options :: [Option], dom :: a, cod :: a, l :: AttachNode}

instance Show a => Show (Draw a) where
    show (Draw i o d c dr) = show i ++ ":\\draw" ++ show o ++ " (" ++ show d ++ ") to " ++ show dr ++  " (" ++ show c ++ ");\n"

instance Show a => Render (Draw a) where
    render (Draw i o d c dr) = let roundbraces x = "(" ++ show x ++ ")"
                               in "\\draw" ++ show o ++ " " ++ roundbraces d ++ " to " ++ show dr ++ roundbraces c ++ ";\n"

instance Functor Draw where
    fmap f (Draw n os d c l) = Draw n os (f d) (f c) l

instance MainKey (Draw a) where
    itsKey = idDraw

instance MainKey ProtoDraw where
    itsKey = idPD

instance Num a => Semigroup (Draw a) where
    (Draw n1 o1 d1 c1 l1) <> (Draw n2 o2 d2 c2 l2) = Draw n2 o1 d2 c2 l1

instance Num a => Monoid (Draw a)where
    mempty = Draw 0 [] 0 0 NoLabel
    mappend = (<>)

data ProtoDraw = PD{idPD :: Int, optionsPD :: [Option], lPD :: AttachNode}deriving(Show)

instance Num a => Action ProtoDraw (Draw a) where
    PD n os lpd <+> Draw m ps d c l' = Draw m os d c lpd

fromPD :: Num a => ProtoDraw -> Draw a
fromPD (PD n os an) = Draw n os 0 0 an

actDraw :: Num a => ProtoDraw -> Draw a -> Draw a
actDraw pd d = fromPD pd `mappend` d

setup :: ([Int],[(Int,Int)]) -> ([Graph Int],[Graph Int])
setup = cross (map Vertex) (map (uncurry Connect . cross Vertex Vertex))

cross :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
cross f g (x,y) = (f x,g y)


planeNode :: Int -> Node
planeNode a = Node a 0 0 ""

planeDraw :: (Int,Int) -> Int -> Draw Int
planeDraw (n,m) x = Draw x [] n m NoLabel

nodedraws :: ([Int],[(Int,Int)]) -> ([Node],[Draw Int])
nodedraws (ns,es) = let es' = zip es [1..] in cross (map planeNode) (map (uncurry planeDraw)) (ns,es')

-- Draw値の更新オペレータ
setOption :: [Option] -> Draw a -> Draw a
setOption os (Draw n ops d c l) = Draw n os d c l

-- Placeつき作用オペレータを作ったのでこちらは凍結
-- setLabel :: String -> Draw a -> Draw a
-- setLabel l (Draw n os d c l') = Draw n os d c l

-- プレーンなDrawのOptionとLabelを更新するオペレータ。setOptLab'はID
-- setOptLab' n op l d = if n == idDraw d then setOption op . setLabel l -- $ d
--                                        else d

-- setOptLab op l d = setOptLab' (idDraw d) op l d

-- IDと紐付けされた関数をDrawに適用する高階関数
evalByID :: (Int -> Draw a -> t) -> Draw a -> t
evalByID f d = let m = idDraw d in f m d


viewData :: Show a => [a] -> IO ()
viewData = putStrLn . concatMap show

-- mkDraws :: Graph Int -> [Draw] --Partial Function
-- mkDraws g = let xs = getEdges g in zipWith planeDraw xs [1,2..]

data Diagram a = Dia [Node] [Draw a] deriving (Show)

-- planeDiagram :: Graph Int -> Diagram
-- planeDiagram g = Dia (map planeNode . getVertices $ g) (concatMap mkDraws . getEdges $ g)

