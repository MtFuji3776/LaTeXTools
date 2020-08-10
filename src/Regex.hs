{-# LANGUAGE OverloadedStrings #-}
module Regex where

import Data.String

data Reg = Eps 
         | Sym Bool Char
         | Alt Reg Reg
         | Seq Reg Reg
         | Rep Reg

instance Show Reg where
    show Eps = ""
    show (Sym b c) = if b == True then return c ++ "'" else return c
    show (Alt r s) = "(" ++ show r ++ "|" ++ show s ++ ")"
    show (Seq r s) = show r ++ show s
    show (Rep r) = case r of Alt s t -> show r ++ "*"; _ -> "(" ++ show r ++ ")*"

instance IsString Reg where
    fromString  = foldr Seq Eps . map (Sym True)

instance Num Reg where
    fromInteger = fromString . show
    (+)         = Alt
    (*)         = Seq
    abs         = id
    negate      = id
    signum      = id

empty :: Reg -> Bool
empty Eps       = True
empty (Sym _ _) = False
empty (Alt p q) = empty p || empty q
empty (Seq p q) = empty p && empty q
empty (Rep r)   = True

final :: Reg -> Bool
final Eps       = False
final (Sym b _) = b
final (Alt p q) = final p || final q
final (Seq p q) = final p && final q
final (Rep r)   = final r

shift :: Bool -> Reg -> Char -> Reg
shift _ Eps _ = Eps
shift m (Sym _ x) c = Sym (m && x == c) x
shift m (Alt p q) c = Alt (shift m p c) (shift m q c)
shift m (Seq p q) c = Seq (shift m p c) (shift (m && empty p || final p) q c)
shift m (Rep r) c = Rep $ shift (m || final r) r c

match :: Reg -> String -> Bool
match r [] = empty r
match r (c:cs) = final $ foldl (shift False) (shift True r c) cs