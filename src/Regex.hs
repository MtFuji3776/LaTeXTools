{-# LANGUAGE OverloadedStrings #-}
module Regex where

import Data.String

data Signal = Before| After | Duplicated deriving(Eq,Show)

judge :: Signal -> Bool
judge Before     = True
judge After      = True
judge Duplicated = False

data Reg = Eps
         | Sym Bool Char
         | Wild Bool
         | Alt Reg Reg
         | Seq Reg Reg
         | Rep Reg
         | Not Reg deriving(Show)
         

-- instance Show Reg where
--     show Eps = ""
--     show (Wild b) = if b then ".t" else ".f"
--     show (Sym b c) = if b then return c ++ "'" else return c
--     show (Alt r s) = "(" ++ show r ++ "|" ++ show s ++ ")"
--     show (Seq r s) = show r ++ show s
--     show (Rep r) = case r of Alt s t -> show r ++ "*"; _ -> "(" ++ show r ++ ")*"
--     show (Not r) = "¬" ++ show r

instance IsString Reg where
    fromString "" = Eps
    fromString xs = foldl1 Seq . map (Sym False) $ xs

instance Num Reg where
    fromInteger = fromString . show
    (+)         = Alt
    (*)         = Seq
    abs         = id
    negate      = Not
    signum      = id

empty :: Reg -> Bool
empty Eps       = True
empty (Wild b)  = False
empty (Sym _ _) = False
empty (Alt p q) = empty p || empty q
empty (Seq p q) = empty p && empty q
empty (Rep r)   = True
empty (Not r)   = not . empty $ r

final :: Reg -> Bool
final Eps       = False
final (Wild b)  = b
final (Sym b _) = b
final (Alt p q) = final p || final q
final (Seq p q) = final p && final q
final (Rep r)   = final r
final (Not r)   = not . final $ r

shift :: Bool -> Reg -> Char -> Reg
shift _ Eps _ = Eps
shift m (Wild _) c = Wild m
shift m (Sym _ x) c = Sym (m && x == c) x
shift m (Alt p q) c = Alt (shift m p c) (shift m q c)
shift m (Seq p q) c = Seq (shift m p c) (shift (m && empty p || final p) q c)
shift m (Rep r) c = Rep $ shift (m || final r) r c
shift m (Not r) c = shift m r c

match :: Reg -> String -> Bool
match r [] = empty r
match r (c:cs) = final $ foldl (shift False) (shift True r c) cs