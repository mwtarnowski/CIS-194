{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- CIS 194 Spring 2013: Homework 5

module Calc where

import ExprT
import Parser
import Control.Applicative (liftA2)

import qualified StackVM  as S
import qualified Data.Map as M


-- Exercise 1 -----------------------------------------

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


-- Exercise 2 -----------------------------------------

evalStr :: String -> Maybe Integer
evalStr xs = case parseExp Lit Add Mul xs of
             Just expr -> Just $ eval expr
             Nothing   -> Nothing

evalStr' :: String -> Maybe Integer
evalStr' = fmap eval . parseExp Lit Add Mul


-- Exercise 3 -----------------------------------------

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul


-- Exercise 4 -----------------------------------------

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer 
    deriving (Eq, Ord, Show) --added Ord

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer 
    deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 n) (Mod7 m) = lit (n + m)
    mul (Mod7 n) (Mod7 m) = lit (n * m)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"
--testInteger  = testExp :: Maybe Integer
--testBool     = testExp :: Maybe Bool
--testMM       = testExp :: Maybe MinMax
--testSat      = testExp :: Maybe Mod7


-- Exercise 5 -----------------------------------------

instance Expr S.Program where
    lit n = [S.PushI n]
    add n m = n ++ m ++ [S.Add]
    mul n m = n ++ m ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul


-- Exercise 6 -----------------------------------------

class HasVars a where
    var :: String -> a

data VarExprT = ExprLit Integer | ExprAdd VarExprT VarExprT | ExprMul VarExprT VarExprT | Var String 
    deriving (Show, Eq)


instance Expr VarExprT where
    lit = ExprLit
    add = ExprAdd
    mul = ExprMul

instance HasVars VarExprT where
    var = Var


-- Since predefined instances of Applicative typeclass
-- include ((->) e) and Maybe we can use liftA2 here
-- for shortness:
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add = liftA2 $ liftA2 (+)
    mul = liftA2 $ liftA2 (*)

-- without using these properties:
{-  add f g x = case f x of
                Nothing -> Nothing
                Just n  -> case g x of
                           Nothing -> Nothing
                           Just m  -> Just (n + m)
    mul f g x = case f x of
                Nothing -> Nothing
                Just n  -> case g x of
                           Nothing -> Nothing
                           Just m  -> Just (n * m) -}

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
