{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Text.LaTeX.Base hiding((<>))
import Text.LaTeX.Base.Syntax hiding((<>))
import Text.LaTeX.Base.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSSymb
import Data.Semigroup
import Control.Monad.Writer.Strict hiding((<>))
import System.Directory
--import Lens.Control


path = (++"/output") <$> getCurrentDirectory

output :: T.Text -> IO ()
output xs = do
    path' <- path
    TIO.writeFile path' xs

type ProtoNode = T.Text

data ProtoArrow = PA{
    dom :: ProtoNode,
    cod :: ProtoNode,
    nameOfPA :: T.Text
} deriving(Eq,Show)

data Node = Node {
    nameOfNode :: T.Text,
    xCoor :: Double,
    yCoor :: Double,
    labelOfNode :: Maybe T.Text
}deriving(Eq,Show)

(-->) :: ProtoNode -> ProtoNode -> T.Text -> Diagram
(-->) p1 p2 n = Mor $ PA p1 p2 n

node n x y l = Obj $ Node n x y l

data Vertical = V{
    yFrom :: Double,
    yTo :: Double,
    quantifier :: Quantifier
} deriving (Eq,Show)

data Quantifier = Forall | Exists | Exists' deriving(Eq,Show)

evalQ :: Quantifier -> LaTeX
evalQ Forall  = let on = OptNode "above" "\\forall" in evalOptNode on
evalQ Exists  = let on = OptNode "above" "\\exists" in evalOptNode on
evalQ Exists' = let on = OptNode "above" "\\exists !" in evalOptNode on

vertical :: Double -> Double -> Quantifier -> Diagram
vertical y1 y2 q = Ver $ V y1 y2 q

vert = vertical 0

fromVal :: (Show a,LaTeXC l) => a -> l
fromVal = raw . T.pack . show

evalVer :: Vertical -> LaTeX
evalVer (V yf yt q) = execLaTeXM $ do
    let rbraces x = between x "(" ")"
    let fromVal = raw . T.pack . show
    let coordinate x y = rbraces $ between "," (fromVal x) (fromVal y)
    --case q of Nothing -> do{raw "\\draw[-Butt Cap] " ; coordinate 0 (yf-0.2) ; " to "   ; coordinate 0 (yt+0.2) ; ";"};
    do{between (raw "&") (raw "\n") (raw "\n") ;raw "\\draw[-Butt Cap] " ; coordinate 0 (yf-0.2) ; " to " ; " " ; coordinate 0 (yt+0.2) ;fromLaTeX (evalQ q) ; ";" ; between (raw "&") (raw "\n") (raw "\n")}

data DrawOption = Emp | Monic | Epic | Cover | Xshift Double | Yshift Double deriving (Eq,Show)

evalDrawOption dopt = execLaTeXM $ do
    raw "\\draw"
    case dopt of Emp -> "";
                 Monic -> "[|-Stealth]"; 
                 Epic  -> "[->Stealth]";
                 Cover -> raw "[-{Stealth[open]}]";
                 Xshift x -> do{"[transform canvas = " ; braces (do{"xshift = " ; fromVal x ;  "pt"}) ; "]"}
                 Yshift y -> do{"[transform canvas = " ; braces (do{"yshift = " ; fromVal y ; "pt"}) ; "]"}

data OptNode = OptNode{
    place :: T.Text,
    labelOfON :: T.Text
}deriving(Eq,Show)

data Symbols = Pullback Double Double Double Double 
    | Equalizer Double Double Double Double 
    | Product Double Double Double Double
    | Reticle Double Double
    deriving(Eq,Show)

evalOptNode :: OptNode -> LaTeX
evalOptNode (OptNode p l) = execLaTeXM $ do
    let rbraces x = between x "(" ")"
    let squarebraces x = between x "[" "]"
    "node" ; squarebraces (raw p) ; " " ; braces (math . raw $ l) ; " "

data Diagram = Obj Node 
    | Mor ProtoArrow 
    | OptMor{ arr :: Diagram,drawOpt :: DrawOption, optNode :: OptNode }
    | Ver Vertical 
    | Syms Symbols
    | Custom T.Text
    | Seq Diagram Diagram 
    | Unit 
    deriving(Eq,Show)

instance IsString Diagram where
    fromString = Custom . T.pack

draw :: DrawOption -> Diagram -> OptNode -> Diagram
draw xs pa optnode = OptMor pa xs optnode

evalDiagram :: Diagram -> LaTeX
evalDiagram (Obj n)     = evalNode n
evalDiagram (Mor pa)    = evalPA pa
evalDiagram (OptMor pa dopt optn) = evalOptMor $ OptMor pa dopt optn
evalDiagram (Ver v)     = evalVer v
evalDiagram (Custom xs) = raw xs
evalDiagram (Seq d1 d2) = evalDiagram d1 <> raw "\n" <> evalDiagram d2
evalDiagram Unit        = mempty


evalNode :: Node -> LaTeX
evalNode (Node n x y lab) = execLaTeXM $ do
    let fromVal = raw . T.pack . show
    raw "\\node (" ; raw n ; ") at "
    "(" ; fromVal x ; "," ; fromVal y ; ") "
    case lab of Nothing -> do{braces ""; raw "; \\fill ("; raw n ; ") circle (2pt);" }; (Just l) -> do{braces . math . raw $ l ; ";"}

evalPA :: ProtoArrow -> LaTeX
evalPA (PA d c n)= execLaTeXM $ do
    let rbraces x = between x "(" ")"
    raw "\\draw " ; rbraces (raw d) ; " to " ; rbraces (raw c) ; ";"

evalOptMor (OptMor (Mor (PA d c n)) dro on) = execLaTeXM $ do
    let squarebraces x = between x "[" "]"
    let rbraces x = between x "(" ")"
    fromLaTeX (evalDrawOption dro) -- \draw[dro]
    " " ; rbraces (raw d) ; " to " ; (fromLaTeX $ evalOptNode on) ; " " ; rbraces (raw c) -- (d) to node [...] {$..$} (c)
    ";"

-- evalDrawOption xs = execLaTeXM $ do
--     raw "\\draw[" ; (raw $ mconcat xs) ; "] "


instance Semigroup Diagram where
    d1 <> d2 = Seq d1 d2

instance Monoid Diagram where
    mempty = Unit
    mappend = (<>)



env1 :: String -> LaTeX -> LaTeX
env1 name content = fromLaTeX $ TeXEnv name [] content

env_diagram :: LaTeX -> LaTeX
env_diagram x =  env1 "center" $ TeXEnv "tikzpicture" [MOptArg ["-Stealth"]] $ between x "\n" "\n"


class (Monoid d,IsString d) => DiagramC d where
    liftListD :: ([Diagram] -> Diagram) -> [d] -> d

instance DiagramC Diagram where
    liftListD = id

fromDiagram :: DiagramC d => Diagram -> d
fromDiagram l = liftListD (const l) []