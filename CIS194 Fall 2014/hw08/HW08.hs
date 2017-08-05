{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Fall 2014: Homework 8

module HW08 where

import Data.List (stripPrefix, sort)
import Data.Maybe (isJust)
import Data.Char (isDigit)
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Control.Monad.Random

-------------------------------------------------------
-- Finger exercises -----------------------------------

-- Exercise 1 -----------------------------------------

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
    where go :: String -> Maybe String
          go [] = Just []
          go cs = let prefix = takeWhile isDigit cs in
                  readMaybe prefix >>= \n ->
                  stripPrefix prefix cs >>= \cs' ->
                  if length (takeWhile (=='a') cs) == n
                  then go cs'
                  else Nothing

-- Exercise 2 -----------------------------------------

specialNumbers :: [Int]
specialNumbers = [n | n <- [5,10..100], n `mod` 7 /= 0]


-------------------------------------------------------
-- Risk -----------------------------------------------

type StdRand = Rand StdGen

type DieRoll = Int

type Army = Int

data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
    deriving Show

-- Exercise 3 -----------------------------------------

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

-- Exercise 4 -----------------------------------------

instance Monoid ArmyCounts where
    mempty = ArmyCounts 0 0
    mappend (ArmyCounts a d) (ArmyCounts a' d') 
        = ArmyCounts (a + a') (d + d')

battleResult :: (DieRoll, DieRoll) -> ArmyCounts
battleResult (na, nd)
    | na > nd   = ArmyCounts ( 0) (-1)
    | otherwise = ArmyCounts (-1) ( 0)

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults nas nds 
    = mconcat $ map battleResult $ zip (rsort nas) (rsort nds)
      where rsort = reverse . sort

-- Exercise 5 -----------------------------------------

battle :: ArmyCounts -> StdRand ArmyCounts
battle ac = do 
    let clamp n m = min n . max m
        a = clamp 0 3 $ attackers ac - 1
        d = clamp 0 2 $ defenders ac
    nas <- replicateM a dieRoll
    nds <- replicateM d dieRoll
    return $ ac <> battleResults nas nds

-- Exercise 6 -----------------------------------------

invade :: ArmyCounts -> StdRand ArmyCounts
invade ac | attackers ac < 2 || defenders ac < 1 = return ac
          | otherwise = battle ac >>= invade

-- Exercise 7 -----------------------------------------

(//) :: Int -> Int -> Double
n // m = fromIntegral n / fromIntegral m

successProb :: ArmyCounts -> StdRand Double
successProb ac = do 
    let allBattles = 1000
    bfs <- replicateM allBattles (invade ac)
    let wonBattles = length $ filter ((0==) . defenders) bfs
    return $ wonBattles // allBattles

