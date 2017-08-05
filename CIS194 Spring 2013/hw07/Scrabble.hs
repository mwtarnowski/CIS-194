{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2013: Homework 7

module Scrabble where
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
    mempty = Score 0
    mappend (Score n) (Score m) = Score (n + m)

score :: Char -> Score
score c | l `elem` "AEILNORSTU" = Score 1
        | l `elem` "DG"         = Score 2
        | l `elem` "BCMP"       = Score 3
        | l `elem` "FHVWY"      = Score 4
        | l `elem` "K"          = Score 5
        | l `elem` "JX"         = Score 8
        | l `elem` "QZ"         = Score 10
        | otherwise             = Score 0
        where l = toUpper c
