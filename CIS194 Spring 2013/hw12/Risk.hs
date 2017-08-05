{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- CIS 194 Spring 2013: Homework 12

module Risk where
import Data.List
import Control.Monad.Random
import Data.Monoid ((<>))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

------------------------------------------------------------
-- Exercise 2

instance Monoid Battlefield where
    mempty = Battlefield 0 0
    mappend (Battlefield a d) (Battlefield a' d') 
        = Battlefield (a + a') (d + d')

battleResult :: (DieValue, DieValue) -> Battlefield
battleResult (na, nd)
    | na > nd   = Battlefield ( 0) (-1)
    | otherwise = Battlefield (-1) ( 0)

battleResults :: [DieValue] -> [DieValue] -> Battlefield
battleResults nas nds 
    = let rsort = reverse . sort in 
      mconcat $ map battleResult $ zip (rsort nas) (rsort nds)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do 
    let clamp n m = min n . max m
        a = clamp 0 3 $ attackers bf - 1
        d = clamp 0 2 $ defenders bf
    nas <- replicateM a die
    nds <- replicateM d die
    return $ bf <> battleResults nas nds

------------------------------------------------------------
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf | attackers bf < 2 || defenders bf < 1 = return bf
          | otherwise = battle bf >>= invade

------------------------------------------------------------
-- Exercise 4

(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do 
    let allBattles = 1000
    bfs <- replicateM allBattles (invade bf)
    let wonBattles = length $ filter ((0==) . defenders) bfs
    return $ wonBattles // allBattles

------------------------------------------------------------
-- Exercise 5

-- using precomputed values:
exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield _ 0) = 1
exactSuccessProb (Battlefield 1 _) = 0
exactSuccessProb (Battlefield a@2 d@1) -- 1 battle (1,1)
    =     21/36 * exactSuccessProb (Battlefield (a - 1) (d - 0))
    +     15/36 * exactSuccessProb (Battlefield (a - 0) (d - 1))
exactSuccessProb (Battlefield a@2 d@_) -- 1 battle (1,2)
    =   161/216 * exactSuccessProb (Battlefield (a - 1) (d - 0))
    +    55/216 * exactSuccessProb (Battlefield (a - 0) (d - 1))
exactSuccessProb (Battlefield a@3 d@1) -- 1 battle (2,1)
    =    91/216 * exactSuccessProb (Battlefield (a - 1) (d - 0))
    +   125/216 * exactSuccessProb (Battlefield (a - 0) (d - 1))
exactSuccessProb (Battlefield a@_ d@1) -- 1 battle (3,1)
    =  441/1296 * exactSuccessProb (Battlefield (a - 1) (d - 0))
    +  855/1296 * exactSuccessProb (Battlefield (a - 0) (d - 1))
exactSuccessProb (Battlefield a@3 d@_) -- 2 battles (2,2)
    =  581/1296 * exactSuccessProb (Battlefield (a - 2) (d - 0))
    +  420/1296 * exactSuccessProb (Battlefield (a - 1) (d - 1))
    +  295/1296 * exactSuccessProb (Battlefield (a - 0) (d - 2))
exactSuccessProb (Battlefield a@_ d@_) -- 2 battles (3,2)
    = 2275/7776 * exactSuccessProb (Battlefield (a - 2) (d - 0))
    + 2611/7776 * exactSuccessProb (Battlefield (a - 1) (d - 1))
    + 2890/7776 * exactSuccessProb (Battlefield (a - 0) (d - 2))

