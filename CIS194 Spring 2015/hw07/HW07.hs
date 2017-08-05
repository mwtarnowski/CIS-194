{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
-- CIS 194 Spring 2015: Homework 7

module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random hiding (mapM, liftM)
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= return . f

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = [f x | x <- mx]

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 replaceWith (v !? i) (v !? j)
              where replaceWith x y = v // [(i, y), (j, x)]

swapV' :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV' i j v = [v // [(i, y), (j, x)] | x <- v !? i, y <- v !? j]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v
    | V.null v  = return Nothing
    | otherwise = do i <- getRandomR (0, V.length v - 1)
                     return $ v !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = V.replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldM randomSwap v $ reverse [1..V.length v - 1]
            where randomSwap w i = do j <- getRandomR (0, i)
                                      return $ w // [(i, w ! j), (j, w ! i)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (ltx, x, gex)
                  where (x, v') = (v ! i, V.take i v V.++ V.drop (i+1) v)
                        (ltx, gex) = V.partition (< x) v'

-- Exercise 7 -----------------------------------------

-- Quicksort
-- In fact, none of the following functions realizes 
-- the real quicksort algorithm by C.A.R.Hoare.
-- See: C.A.R.Hoare, Quicksort, The Computer Journal 5, 1962.
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v | V.null v = v
qsort v = qsort ltx <> (x `cons` qsort gex)
          where (ltx, x, gex) = partitionAt v 0


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v | V.null v = return v
qsortR v = do i <- getRandomR (0, V.length v - 1)
              let (ltx, x, gex) = partitionAt v i
              ltx' <- qsortR ltx
              gex' <- qsortR gex
              return $ ltx' <> (x `cons` gex')

-- Exercise 9 -----------------------------------------

select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select _ v | V.null v = return Nothing
select n v = do i <- getRandomR (0, V.length v - 1)
                let (ltx, x, gex) = partitionAt v i
                    k = length ltx
                case compare n k of
                    LT -> select n ltx
                    GT -> select (n-k-1) gex
                    EQ -> return $ Just x

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | l <- labels, s <- suits]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d 
    | V.null d  = Nothing
    | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n _ | n < 0 = Nothing
getCards 0 d = Just ([], d)
getCards n d = do (c, d') <- nextCard d
                  (cs, d'') <- getCards (n-1) d'
                  return (c:cs, d'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
