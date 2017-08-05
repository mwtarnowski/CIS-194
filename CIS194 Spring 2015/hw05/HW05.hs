{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- CIS 194 Spring 2015: Homework 5

module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser


-- Exercise 1 -----------------------------------------

decodeBytes :: ByteString -> ByteString -> ByteString
decodeBytes bs bs' = BS.pack $ BS.zipWith xor bs bs'

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path path' = do
    org <- BS.readFile path
    enc <- BS.readFile path'
    return $ BS.filter (/= 0) $ decodeBytes org enc


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    enc <- BS.readFile $ path ++ ".enc"
    BS.writeFile path $ decodeBytes enc $ BS.cycle key


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = fmap decode . BS.readFile


-- Exercise 4 -----------------------------------------

filterBadTs :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterBadTs (Just ids) = fmap $ filter (flip elem ids . tid)
filterBadTs  _         = const Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs path path' = do
    mids <- parseFile path  :: IO (Maybe [TId])
    mts  <- parseFile path' :: IO (Maybe [Transaction])
    return $ filterBadTs mids mts


-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr pushTransaction Map.empty
          where pushTransaction t = 
                    Map.insertWith (+) (to   t) (         amount t) . 
                    Map.insertWith (+) (from t) (negate $ amount t)


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey trackCriminal ("Unknown", 0)
    where trackCriminal name income (name', income')
              | income > income' = (,) name  income
              | otherwise        = (,) name' income'


-- Exercise 7 -----------------------------------------

payBack :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
payBack (payer:payers) (payee:payees) (tid:tids) = 
    (Transaction (fst payer) (fst payee) payment tid) : (payBack payers' payees' tids)
    where payment = min (snd payer) (snd payee)
          (payers', payees') = if (snd payer < snd payee) 
                               then (payers, subtract payment <$> payee : payees)
                               else (subtract payment <$> payer : payers, payees)
payBack _ _ _ = []


undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m = reverse . payBack (payers m) (payees m)
           where payers = toSortedList .                  Map.filter (> 0)
                 payees = toSortedList . Map.map negate . Map.filter (< 0)
                 toSortedList = reverse . sortBy (comparing snd) . Map.toList


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path = BS.writeFile path . encode


-- Exercise 9 -----------------------------------------

process :: (FilePath, FilePath, FilePath, FilePath, FilePath, FilePath) -> IO String
process (photo, photo', trans, decIds, newIds, out) = do
    key <- getSecret photo photo'
    decryptWithKey key decIds
    mts  <- getBadTs decIds trans
    mids <- parseFile newIds :: IO (Maybe [TId])
    case (mts, mids) of
        (Just ts, Just ids) -> do
            let flow = getFlow ts
            writeJSON out $ undoTs flow ids
            return $ getCriminal flow
        _ -> return "Invalid data"


main :: IO ()
main = getArgs >>= process . prepArgs >>= putStrLn
       where prepArgs (arg0 : arg1 : arg2 : arg3 : arg4 : arg5 : _) = 
                 (arg0, arg1, arg2, arg3, arg4, arg5)
             prepArgs _ = 
                 ( "dog-original.jpg"
                 , "dog.jpg"
                 , "transactions.json"
                 , "victims.json"
                 , "new-ids.json"
                 , "new-transactions.json" )

