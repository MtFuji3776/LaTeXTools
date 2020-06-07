{-# LANGUAGE OverloadedStrings #-}

import Syntax
import Writer
import Control.Monad.Writer.Strict
import Text.LaTeX.Base(render)

preadjoint = execWriter $ do
    let n1 = execWriter $ do
            tell $ node "a" 2 2 $ Just "A"
            tell $ node "gb" 0 0 $ Just "G(B)"
        a1 = execWriter $ do
            tell $ "a" --> "gb" $ ""
        d1 = execWriter $ do
            tell $ vert 2 Forall
            tell n1
            tell a1
        n2 = execWriter $ do
            tell $ node "gbi" 4 0 $ Just "G(B_i)"
        a2 = execWriter $ do
            tell $ "a" --> "gb" $ ""
            tell $ draw Emp ("gbi" --> "gb" $ "") $ OptNode "below" "Gx"
        d2 = execWriter $ do
            tell d1
            tell $ vert 2 Exists
            tell n2
            tell a2
    tell d2

adjoint :: Writer Diagram ()
adjoint = do
    let d1 = do
            tell $ node "a" 0 2 $ Just "A"
            tell $ node "gb" 2 0 $ Just "GB"
            tell $ "a" --> "gb" $ ""
        d2 = do
            d1
            tell $ node "gfa" 2 2 $ Just "GFA"
            tell $ draw Emp ("a" --> "gfa" $ "") $ OptNode "above" "\\eta_A"
        d3 = do
            d2
            tell $ draw Emp ("gfa" --> "gb" $ "") $ OptNode "right" "Gx"
    tell $ vert 2 Forall
    d1
    tell $ vert 2 Exists
    d2
    tell $ vert 2 Exists'
    d3