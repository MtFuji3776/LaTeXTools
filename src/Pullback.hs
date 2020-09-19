{-# LANGUAGE OverloadedStrings #-}

module Pullback where

import Syntax
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Writer.Strict


-- pullbackDef = do
d1 :: Writer Diagram ()
d1 = do
        tell $ node "x" 0 0 Nothing
        tell $ node "y" 2 2 Nothing
        tell $ node "z" 2 0 Nothing
        tell $ "x" --> "z" $ ""
        tell $ "y" --> "z" $ ""

d2 :: Writer Diagram ()
d2 = do
    d1
    tell $ node "p" 0 2 Nothing
    tell $ "p" --> "x" $ ""
    tell $ "p" --> "y" $ ""

d3 = do
    d2
    tell $ node "w" (-1) 3 Nothing
    tell $ "w" --> "x"$ ""
    tell $ "w" --> "y"$ ""

d4 = do
    d3
    tell $ "w" --> "p" $ ""

pullbackDef :: Writer Diagram ()
pullbackDef = do
    d2
    tell $ vert 3 Forall
    d3
    tell $ vert 3 Exists'
    d4

execDiagram :: Writer Diagram () -> IO ()
execDiagram w = let x = (render . evalDiagram . execWriter $ w) in TIO.putStrLn x >> output x


catWhichHavePullBack :: Writer Diagram ()
catWhichHavePullBack = do
    let v x = tell $ vert 3 x :: Writer Diagram ()
    v Forall
    d1
    v Exists
    d2
    v Forall
    d3
    v Exists'
    d4



q1_1 :: Writer Diagram ()
q1_1 = do
    tell $ node "p" 0 2 $ Just "P"
    tell $ node "x" 0 0 Nothing
    tell $ node "y" 2 2 Nothing
    tell $ node "z" 2 0 Nothing
    tell $ draw Emp ("x" --> "z" $ "") $ OptNode "below" "f"
    tell $ draw Emp ("y" --> "z" $ "") $ OptNode "right" "g"
    tell $ draw Emp ("p" --> "x" $ "") $ OptNode "left" "l"
    tell $ draw Emp ("p" --> "y" $ "") $ OptNode "above" "r"

q1_2 :: Writer Diagram ()
q1_2 = do
    tell $ node "w" 2 4 $ Just "W"
    tell $ node "p" 2 2 $ Just "P"
    tell $ node "x" 0 0 Nothing
    tell $ node "y" 4 0 Nothing
    tell $ draw Emp ("p" --> "x" $ "") $ OptNode "left" "l"
    tell $ draw Emp ("p" --> "y" $ "") $ OptNode "right" "r"
    tell $ draw Emp ("w" --> "x" $ "") $ OptNode "left" "x_1"
    tell $ draw Emp ("w" --> "y" $ "") $ OptNode "right" "x_2"
    tell $ draw Emp ("w" --> "p" $ "") $ OptNode "left" "\\phi_1"
    tell $ draw Emp ("w" --> "p" $ "") $ OptNode "right" "\\phi_2"