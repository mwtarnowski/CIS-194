{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- CIS 194 Fall 2014: Homework 6

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T


-- Exercise 1 -----------------------------------------

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object o)   = Object $ fmap ynToBool o
ynToBool (Array  a)   = Array  $ fmap ynToBool a
ynToBool value        = value

-- Exercise 2 -----------------------------------------

parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- Exercise 3 -----------------------------------------

data Market = Market { marketname :: T.Text
                     , posX       :: Double
                     , posY       :: Double
                     , state      :: T.Text }
    deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = 
    case fmap fromJSON (parseData bs) of
        (Left s)            -> Left s
        (Right (Error s))   -> Left s
        (Right (Success a)) -> Right a

-- Exercise 4 -----------------------------------------

loadData :: IO [Market]
loadData = B.readFile path >>= either fail return . parseMarkets
           where path = "markets.json"

-- Exercise 5 -----------------------------------------

data OrdList a = OrdList { getOrdList :: [a] }
    deriving (Eq, Show)

mergeOrd :: (Ord a) => [a] -> [a] -> [a]
mergeOrd xs [] = xs
mergeOrd [] ys = ys
mergeOrd xs@(x:xs') ys@(y:ys') 
    | x <= y    = x : mergeOrd xs' ys
    | otherwise = y : mergeOrd xs  ys'

instance (Ord a) => Monoid (OrdList a) where
    mempty = OrdList []

    mappend (OrdList xs) (OrdList ys) = 
        OrdList $ mergeOrd xs ys

    mconcat []   = mempty
    mconcat [xs] = xs
    mconcat xss@(_:xss') = 
        mconcat (everyother xss) <> mconcat (everyother xss')
        where everyother (y:_:ys) = y : everyother ys
              everyother  ys      = ys

-- Exercise 6 -----------------------------------------

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f txt = mconcat . map f . filter matchesName
               where matchesName = T.isInfixOf txt . marketname

-- Exercise 7 -----------------------------------------

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.).(.)

firstFound :: Searcher (Maybe Market)
firstFound = getFirst `compose2` search (First . Just)

-- Exercise 8 -----------------------------------------

lastFound :: Searcher (Maybe Market)
lastFound = getLast `compose2` search (Last . Just)

-- Exercise 9 -----------------------------------------

allFound :: Searcher [Market]
allFound = search (:[])

-- Exercise 10 ----------------------------------------

numberFound :: Searcher Int
numberFound = getSum `compose2` search (const $ Sum 1)

-- Exercise 11 ----------------------------------------

newtype NtoSMarket = NtoS Market
    deriving (Eq, Show)

-- Warning: note that there is no implication:
-- compare m n == EQ  =>  m == n
instance Ord NtoSMarket where
    compare (NtoS m) (NtoS n)
        | posY m == posY n = posX m `compare` posX n
        | otherwise        = posY m `compare` posY n

orderedNtoS :: Searcher [NtoSMarket]
orderedNtoS = getOrdList `compose2` search (\m -> OrdList [NtoS m])

