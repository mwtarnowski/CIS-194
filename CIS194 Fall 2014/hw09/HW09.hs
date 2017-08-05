{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- CIS 194 Fall 2014: Homework 9

module HW09 where

import Control.Monad (liftM, replicateM)
import Test.QuickCheck
import Test.HUnit
import Ring

-- Testing Ring properties ----------------------------

-- Exercises 1 2 --------------------------------------

instance Arbitrary Mod5 where
    arbitrary = liftM MkMod $ choose (0,4)


instance Arbitrary Mat2x2 where
    arbitrary = do 
        [a, b, c, d] <- replicateM 4 arbitrary
        return $ MkMat a b c d

    {-arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ MkMat a b c d-}

    shrink (MkMat a b c d) =
        [MkMat x b c d | x <- shrink a] ++
        [MkMat a x c d | x <- shrink b] ++
        [MkMat a b x d | x <- shrink c] ++
        [MkMat a b c x | x <- shrink d]


-- Exercise 3 -----------------------------------------

-- Prop.1: Additive associativity
prop_1 :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_1 x y z = (x `add` y) `add` z == x `add` (y `add` z)

-- Prop.2: Additive commutativity
prop_2 :: (Eq a, Ring a) => a -> a -> Bool
prop_2 x y = x `add` y == y `add` x

-- Prop.3: Additive identity
prop_3 :: (Eq a, Ring a) => a -> Bool
prop_3 x = x `add` addId == x

-- Prop.4: Additive inverse
prop_4 :: (Eq a, Ring a) => a -> Bool
prop_4 x = x `add` addInv x == addId

-- Prop.5: Multiplicative associativity
prop_5 :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_5 x y z = (x `mul` y) `mul` z == x `mul` (y `mul` z)

-- Prop.6: Multiplicative (left) identity
prop_6 :: (Eq a, Ring a) => a -> Bool
prop_6 x = mulId `mul` x == x

-- Prop.7: Multiplicative (right) identity
prop_7 :: (Eq a, Ring a) => a -> Bool
prop_7 x = x `mul` mulId == x

-- Prop.8: Left distributivity
prop_8 :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_8 x y z = x `mul` (y `add` z) == (x `mul` y) `add` (x `mul` z)

-- Prop.9: Right distributivity
prop_9 :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_9 x y z = (x `add` y) `mul` z == (x `mul` z) `add` (y `mul` z)


-- Exercise 4 -----------------------------------------

prop_ring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_ring x y z = 
    conjoin [prop x y z | prop <- [prop_1, prop_5, prop_8, prop_9]] .&&.
    conjoin [prop x y   | prop <- [prop_2]                        ] .&&.
    conjoin [prop x     | prop <- [prop_3, prop_4, prop_6, prop_7]]


-- Exercise 5 -----------------------------------------

-- Booleans does not form a ring. Prop. 4 is violated: 
-- there is no inverse element for True, since 
-- x || True == True for any x of Bool type.
-- Such operations constitute, however, a semiring.


-- Generating binary search trees ---------------------


data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving Show


-- Exercise 6 -----------------------------------------

-- | Is the tree a BST between the given endpoints?
isBSTBetween' :: (Ord a) => Maybe a -> Maybe a -> BST a -> Bool
isBSTBetween' _       _       Leaf = True
isBSTBetween' m_lower m_upper (Node left x right)
  = isBSTBetween' m_lower  (Just x) left  &&
    isBSTBetween' (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower <= x
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x <= upper
      Nothing    -> True

-- | Is this a valid BST?
isBST' :: (Ord a) => BST a -> Bool
isBST' = isBSTBetween' Nothing Nothing


-- Exercises 7 8 --------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (BST a) where
    arbitrary = do
        x <- arbitrary
        y <- suchThat arbitrary (>= x)
        sized $ genBST x y

genBST :: (Ord a, Arbitrary a) => a -> a -> Int -> Gen (BST a)
genBST _ _ 0 = return Leaf
genBST x y _ | x > y = return Leaf
genBST x y n = frequency 
    [ (1, return Leaf)
    , (2, do z <- suchThat arbitrary (\z -> x <= z && z <= y)
             left  <- genBST x z (n `div` 2)
             right <- genBST z y (n `div` 2)
             return $ Node left z right)]


-- Testing parsers ------------------------------------

-- Exercise 9 -----------------------------------------

parserTests :: Test
parserTests = 
    TestList [ "Integer: one digit number" ~:
               parseAll "1" ~?= Just (1 :: Integer)
             , "Integer: more digits number" ~:
               parseAll "37" ~?= Just (37 :: Integer)
             , "Integer: negative number" ~:
               parseAll "-37" ~?= Just (-1 :: Integer)
             , "Mod5: one digit number" ~:
               parseAll "1" ~?= Just (MkMod 1)
             , "Mod5: more digits number" ~:
               parseAll "37" ~?= Just (MkMod 2)
             , "Mod5: negative number" ~:
               parseAll "-37" ~?= Just (MkMod 3)
             , "Mat2x2: all same" ~:
               parseAll "[[0,0][0,0]]" ~?= Just (MkMat 0 0 0 0)
             , "Mat2x2: all different" ~:
               parseAll "[[1,2][3,4]]" ~?= Just (MkMat 1 2 3 4)
             , "Bool: True" ~:
               parseAll "True" ~?= Just True
             , "Bool: False" ~:
               parseAll "False" ~?= Just False
             ]

