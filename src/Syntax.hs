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
--import Lens.Control


path = "/Users/fujimotomakoto/documents/latexs/output"

output :: T.Text -> IO ()
output xs = TIO.writeFile path xs

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
    labelOfNode :: Maybe LaTeX
}deriving(Eq,Show)

(-->) :: ProtoNode -> ProtoNode -> T.Text -> ProtoArrow
(-->) p1 p2 n = PA p1 p2 n

node = Node

data Vertical = V{
    yFrom :: Double,
    yTo :: Double,
    quantifier :: Maybe T.Text
} deriving (Eq,Show)

fromVal :: (Show a,LaTeXC l) => a -> l
fromVal = raw . T.pack . show

evalVer :: Vertical -> LaTeX
evalVer (V yf yt q) = execLaTeXM $ do
    let rbraces x = between x "(" ")"
    let fromVal = raw . T.pack . show
    let coordinate x y = rbraces $ between "," (fromVal x) (fromVal y)
    case q of Nothing -> do{raw "\\draw[-Butt Cap] " ; coordinate 0 (yf-0.2) ; " to "   ; coordinate 0 (yt+0.2) ; ";"};
              Just x -> do{raw "\\draw[-Butt Cap] " ; coordinate 0 (yf-0.2) ; " to " ; " " ; coordinate 0 (yt+0.2) ; "node[above] "; braces (math . raw $ x) ; ";"}

data DrawOption = Monic | Epic | Cover | Xshift Double | Yshift Double deriving (Eq,Show)

evalDrawOption dopt = execLaTeXM $ do
    raw "\\draw"
    case dopt of Monic -> "[|-Stealth]"; 
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
    | OptMor{ arr :: ProtoArrow,drawOpt :: DrawOption, optNode :: OptNode }
    | Ver Vertical 
    | Syms Symbols
    | Seq Diagram Diagram 
    | Unit 
    deriving(Eq,Show)

draw :: DrawOption -> ProtoArrow -> OptNode -> Diagram
draw xs pa optnode = OptMor pa xs optnode

evalDiagram :: Diagram -> LaTeX
evalDiagram (Obj n)     = evalNode n
evalDiagram (Mor pa)    = evalPA pa
evalDiagram (OptMor pa dopt optn) = evalOptMor $ OptMor pa dopt optn
evalDiagram (Ver v)     = evalVer v
evalDiagram (Seq d1 d2) = evalDiagram d1 <> raw "\n" <> evalDiagram d2
evalDiagram Unit        = mempty


evalNode :: Node -> LaTeX
evalNode (Node n x y lab) = execLaTeXM $ do
    let fromVal = raw . T.pack . show
    raw "\\node (" ; raw n ; ") at "
    "(" ; fromVal x ; "," ; fromVal y ; ") "
    case lab of Nothing -> do{braces ""; raw "; \\fill ("; raw n ; ") circle (2pt);" }; (Just l) -> do{braces . math . fromLaTeX $ l ; ";"}

evalPA :: ProtoArrow -> LaTeX
evalPA (PA d c n)= execLaTeXM $ do
    let rbraces x = between x "(" ")"
    raw "\\draw " ; rbraces (raw d) ; " to " ; rbraces (raw c) ; ";"

evalOptMor (OptMor (PA d c n) dro on) = execLaTeXM $ do
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


