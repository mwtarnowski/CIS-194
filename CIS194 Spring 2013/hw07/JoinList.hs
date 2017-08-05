{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- CIS 194 Spring 2013: Homework 7

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor


-- Exercise 1 -----------------------------------------

data JoinList m a = Empty 
                  | Single m a 
                  | Append m (JoinList m a) (JoinList m a) 
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lst lst' = Append (tag lst <> tag lst') lst lst'


-- Exercise 2 -----------------------------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ x) 
    | i == 0    = Just x
    | otherwise = Nothing
indexJ i (Append t left right)
    | i < j     = indexJ i left
    | i < k     = indexJ (i-j) right
    | otherwise = Nothing
    where j = getSize . size $ tag left
          k = getSize . size $ t


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i lst | i <= 0 = lst
dropJ i (Append t left right)
    | i < j = dropJ i left +++ right 
    | i < k = dropJ (i-j) right
    where j = getSize . size $ tag left
          k = getSize . size $ t
dropJ _ _ = Empty


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i <= 0 = Empty
takeJ i (Append t left right)
    | i < j = takeJ i left
    | i < k = left +++ takeJ (i-j) right
    where j = getSize . size $ tag left
          k = getSize . size $ t
takeJ _ lst = lst


-- Exercise 3 -----------------------------------------

scoreString :: String -> Score
scoreString = mconcat . map score


-- Exercise 4 -----------------------------------------

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ cs) = cs
    toString (Append _ left right) = unlines [toString left, toString right]
    fromString = foldr (+++) Empty . map (\cs -> Single (scoreString cs, Size 1) cs) . lines
    line = indexJ
    replaceLine i cs lst = takeJ i lst +++ fromString cs +++ dropJ (i+1) lst
    numLines = getSize . snd . tag
    value = getScore . fst . tag


main :: IO ()
main = runEditor editor (fromString $ unlines 
       [ "This buffer is for notes you don't want to save, and for"
       , "evaluation of steam valve coefficients."
       , "To load a different file, type the character L followed"
       , "by the name of the file."
       ] :: JoinList (Score, Size) String)
