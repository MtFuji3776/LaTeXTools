{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Text.LaTeX.Base
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test = "test" :: LaTeX

path = "/Users/fujimotomakoto/documents/latexs/output"