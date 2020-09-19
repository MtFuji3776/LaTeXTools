{-# LANGUAGE OverloadedStrings #-}

module Templates where
    

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

preamble :: LaTeX
preamble = execLaTeXM $ do
    documentclass [CustomOption "dvipdfmx",CustomOption "uplatex"] "jsarticle"   ; "\n"
    usepackage ["utf8"] "inputenc"  ; "\n"
    usepackage ["epsilon"] "backnaur"  ; "\n"
    usepackage ["dvipdfmx"] "hyperref"  ; "\n"
    mconcat $ map  (\x -> usepackage [] x <> "\n") ["otf",
                               "booktabs",
                               "array",
                               "graphicx",
                               "mathpazo",
                               "amsmath",
                               "amsthm",
                               "amsfonts",
                               "amssymb",
                               "enumitem",
                               "tikz",
                               "bussproofs",
                               "pxjahyper"]
