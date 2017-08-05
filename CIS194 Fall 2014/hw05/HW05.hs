{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- CIS 194 Fall 2014: Homework 5

module HW05 where

import Data.Maybe (listToMaybe)
import Ring
import Parser

-- Exercise 1 -----------------------------------------

intParsingWorks :: Bool
intParsingWorks = 
    (parse "3" == Just (3 :: Integer, "")) && 
    (parseRing "1 + 2 * 5" == Just (11 :: Integer)) && 
    (addId == (0 :: Integer))

-- Exercise 2 -----------------------------------------

data Mod5 = MkMod Integer
    deriving (Show, Eq)

instance Ring Mod5 where
    addId = MkMod 0
    mulId = MkMod 1
    addInv (MkMod n) = MkMod (5 - n)
    add (MkMod n) (MkMod m) = MkMod ((n + m) `mod` 5)
    mul (MkMod n) (MkMod m) = MkMod ((n * m) `mod` 5)

instance Parsable Mod5 where
    parse str = case listToMaybe . reads $ str of
                Just (n, str') -> Just (MkMod (n `mod` 5), str')
                Nothing        -> Nothing

mod5ParsingWorks :: Bool
mod5ParsingWorks = 
    (parse "3" == Just (MkMod 3, "")) &&
    (parse "9" == Just (MkMod 4, "")) &&
    (parseRing "3 + 9" == Just (MkMod 2)) &&
    (parseRing "3 * 9" == Just (MkMod 2))

-- Exercise 3 -----------------------------------------

data Mat2x2 = Mat2x2 Integer Integer Integer Integer
    deriving (Show, Eq)

instance Ring Mat2x2 where
    addId = Mat2x2 0 0 0 0
    mulId = Mat2x2 1 0 0 1
    addInv (Mat2x2 a b c d) 
          = Mat2x2 (-a) (-b) (-c) (-d)
    add (Mat2x2 a b c d) (Mat2x2 a' b' c' d') 
          = Mat2x2 (a + a') (b + b') (c + c') (d + d')
    mul (Mat2x2 a b c d) (Mat2x2 a' b' c' d')
          = Mat2x2 (a * a' + b * c') (a * b' + b * d') 
                   (c * a' + d * c') (c * b' + d * d')

instance Parsable Mat2x2 where
    parse str = case listToMaybe . reads $ str of
                Just ([[a,b],[c,d]], str') -> Just (Mat2x2 a b c d, str')
                _                          -> Nothing

mat2x2ParsingWorks :: Bool
mat2x2ParsingWorks = 
    (parse "[[1,2],[3,4]]" == Just ((Mat2x2 1 2 3 4), "")) &&
    (parseRing "[[1,2],[3,4]] + [[4,3],[2,1]]"   == Just (Mat2x2 5 5 5 5)) &&
    (parseRing "[[0,1],[-1,0]] + [[0,-1],[1,0]]" == Just (addId :: Mat2x2)) &&
    (parseRing "[[1,2],[3,4]] * [[4,3],[2,1]]"   == Just (Mat2x2 8 5 20 13)) &&
    (parseRing "[[0,1],[-1,0]] * [[0,-1],[1,0]]" == Just (mulId :: Mat2x2))

-- Exercise 4 -----------------------------------------

instance Ring Bool where
    addId = False
    mulId = True
    addInv = id
    add = (/=)
    mul = (&&)

instance Parsable Bool where
    parse = listToMaybe . reads

boolParsingWorks :: Bool
boolParsingWorks = 
    (parse "True"  == Just (True, "")) &&
    (parse "False" == Just (False, "")) &&
    (parseRing "True + False" == Just True) &&
    (parseRing "True + True"  == Just False) &&
    (parseRing "True * False" == Just False) &&
    (parseRing "True * True"  == Just True)

-- Exercise 5 -----------------------------------------

distribute :: RingExpr a -> RingExpr a
distribute AddId = AddId
distribute MulId = MulId
distribute (Lit n) = Lit n
distribute (AddInv x) = AddInv (distribute x)
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (Mul (Add s t) y) = distribute (Add (distribute (Mul s y)) (distribute (Mul t y)))
distribute (Mul x (Add s t)) = distribute (Add (distribute (Mul x s)) (distribute (Mul x t)))
distribute (Mul x y) = Mul (distribute x) (distribute y)

-- Exercise 6 -----------------------------------------

squashMulId :: (Eq a, Ring a) => RingExpr a -> RingExpr a
squashMulId AddId = AddId
squashMulId MulId = MulId
squashMulId (Lit n) = Lit n
squashMulId (AddInv x) = AddInv (squashMulId x)
squashMulId (Add x y) = Add (squashMulId x) (squashMulId y)
squashMulId (Mul x (Lit n)) | n == mulId = squashMulId x
squashMulId (Mul (Lit n) y) | n == mulId = squashMulId y
squashMulId (Mul x y) = Mul (squashMulId x) (squashMulId y)

-- Exercise 7 -----------------------------------------

transform :: (RingExpr a -> RingExpr a) -> RingExpr a -> RingExpr a
transform f AddId = f AddId
transform f MulId = f MulId
transform f (Lit n) = f (Lit n)
transform f (AddInv x) = f (AddInv (transform f x))
transform f (Add x y)  = f (Add (transform f x) (transform f y))
transform f (Mul x y)  = f (Mul (transform f x) (transform f y))

-----------

expand :: RingExpr a -> RingExpr a
expand (Mul (Add s t) (Add u v)) = Add (Add (Mul s u) (Mul s v)) (Add (Mul t u) (Mul t v))
expand (Mul (Add s t) y) = Add (Mul s y) (Mul t y)
expand (Mul x (Add s t)) = Add (Mul x s) (Mul x t)
expand expr = expr

simplify :: (Eq a, Ring a) => RingExpr a -> RingExpr a
simplify (Mul x (Lit n)) | n == mulId = x
simplify (Mul (Lit n) y) | n == mulId = y
simplify expr = expr

distribute' :: RingExpr a -> RingExpr a
distribute' = transform expand

squashMulId' :: (Eq a, Ring a) => RingExpr a -> RingExpr a
squashMulId' = transform simplify
