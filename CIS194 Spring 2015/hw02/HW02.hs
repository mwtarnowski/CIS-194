{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2015: Homework 2

module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches s g = length $ filter id $ zipWith (==) s g

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors s = map (\c -> length $ filter (==c) s) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches s g = sum $ zipWith min (countColors s) (countColors g)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g (exactMatches s g) (nonExactMatches s g)
              where nonExactMatches s' g' = matches s' g' - exactMatches s' g'

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move g _ _) g' = move == getMove g' g

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n | n > 0 = concatMap (\c -> [c:cs | cs <- allCodes (n-1)]) colors
allCodes _ = []

-- Exercise 7 -----------------------------------------

solveHelper :: Code -> [Code] -> [Move]
solveHelper _ [] = []
solveHelper s (g:gs) = move : solveHelper s (filterCodes move gs)
                       where move = getMove s g

solve :: Code -> [Move]
solve s = solveHelper s $ allCodes (length s)

