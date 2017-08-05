{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2015: Homework 4

module HW04 where
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 -----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P ps) == (P []) = all (==0) ps
    (P []) == (P ps) = (P ps) == (P [])
    (P (p:ps)) == (P (q:qs)) = p == q && (P ps) == (P qs)
 
-- Exercise 3 -----------------------------------------

showPoly :: (Num a, Eq a, Show a) => [a] -> String
showPoly = intercalate " + " . reverse . map showTerm . filter ((0/=).snd) . zip [0..]
           where showTerm :: (Num a, Eq a, Show a) => (Int, a) -> String
                 showTerm (0, p) = show p
                 showTerm (1, 1) =            "x"
                 showTerm (1,-1) =           "-x"
                 showTerm (1, p) = show p ++  "x"
                 showTerm (n, 1) =            "x^" ++ show n
                 showTerm (n,-1) =           "-x^" ++ show n
                 showTerm (n, p) = show p ++  "x^" ++ show n

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P ps) | deg == 0  = "0"
                | otherwise = showPoly ps
                where deg = length $ dropWhileEnd (==0) ps

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P ps) (P qs) = P (zipWith (+) ps' qs')
                     where ps' = ps ++ replicate diff 0
                           qs' = qs ++ replicate (-diff) 0
                           diff = length qs - length ps

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times (P (p:ps)) (P qs) = P (map (p*) qs) `plus` ((P ps) `times` (P (0:qs)))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P ps) = P (map negate ps)
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P []) _ = 0
applyP (P (p:ps)) q = p + q * applyP (P ps) q

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n | n > 0 = deriv . nderiv (n-1)
    nderiv _ = id

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P ps) = P (drop 1 $ zipWith ((*).fromInteger) [0..] ps)

