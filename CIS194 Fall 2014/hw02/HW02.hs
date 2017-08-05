{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Fall 2014: Homework 2

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

-- Exercise 1 -----------------------------------------

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (c:cs) h = c `elem` h && formableBy cs (delete c h)

-- Exercise 2 -----------------------------------------

wordsFrom :: Hand -> [String]
wordsFrom h = filter (`formableBy` h) allWords

-- Exercise 3 -----------------------------------------

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate (t:ts) h (c:cs)
    | t == '?' = c `elem` h && wordFitsTemplate ts (delete c h) cs
    | t == c   = wordFitsTemplate ts h cs
wordFitsTemplate _ _ _ = False

-- Exercise 4 -----------------------------------------

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate ts h = filter (wordFitsTemplate ts h) allWords

-- Exercise 5 -----------------------------------------

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

-- Exercise 6 -----------------------------------------

bestWordsAcc :: Int -> [String] -> [String] -> [String]
bestWordsAcc _ acc [] = acc
bestWordsAcc n acc (cs:css)
    | n < k     = bestWordsAcc k (cs:[]) css
    | n == k    = bestWordsAcc k (cs:acc) css
    | otherwise = bestWordsAcc n acc css
    where k = scrabbleValueWord cs

bestWords :: [String] -> [String]
bestWords = bestWordsAcc 0 []

-- Exercise 7 -----------------------------------------

scrabbleValueTemplateAcc :: Int -> Int -> STemplate -> String -> Int
scrabbleValueTemplateAcc m n [] [] = m * n
scrabbleValueTemplateAcc m n (t:ts) (c:cs)
    | t == '3'  = scrabbleValueTemplateAcc (3 * m) (1 * k + n) ts cs
    | t == '2'  = scrabbleValueTemplateAcc (2 * m) (1 * k + n) ts cs
    | t == 'T'  = scrabbleValueTemplateAcc (1 * m) (3 * k + n) ts cs
    | t == 'D'  = scrabbleValueTemplateAcc (1 * m) (2 * k + n) ts cs
    | otherwise = scrabbleValueTemplateAcc (1 * m) (1 * k + n) ts cs
    where k = scrabbleValue c
scrabbleValueTemplateAcc _ _ _ _ = 0

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate = scrabbleValueTemplateAcc 1 0

