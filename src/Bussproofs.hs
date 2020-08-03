module Bussproofs where

import Graph

numberOfForm :: Int -> String
numberOfForm 0 = "\\AxiomC"
numberOfForm 1 = "\\UnaryInf"
numberOfForm 2 = "\\BinaryInf"
numberOfForm 3 = "\\TrinaryInf"
numberOfForm 4 = "\\QuaternaryInf"
numberOfForm 5 = "\\QuinaryInf" 
numberOfForm _ = ""

-- 1. Graph Int型でちゃっちゃと木構造を作る→generate関数でRose Int値に変換。
-- 2. Int -> String関数でラベルの内容を決定し、fmapでRose Int -> Rose Stringに昇格。適用すれば導出木ができている。
-- 3. putStrLnなりwriteFileなりお好きな形で出力。

mkBussP :: Rose a -> String
mkBussP (Rose x ts) = let n = length ts in concatMap mkBussP ts ++ numberOfForm n ++ "{$" ++ show x ++ "$}\n"

--ToDO:Sequent Calculi向けのデータ型と関数も用意するべし