{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- CIS 194 Spring 2013: Homework 8

module Party where
import Data.List
import Data.Tree
import Employee

-- Exercise 1 -----------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es f) (GL es' f') = GL (es ++ es') (f + f') --for disjoint lists only

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2 -----------------------------------------

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) $ map (treeFold f) (subForest t)

-- Exercise 3 -----------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e glps = (e `glCons` mconcat (map snd glps), mconcat (map pmax glps))
                   where pmax = uncurry moreFun

-- Exercise 4 -----------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5 -----------------------------------------

glFormat :: GuestList -> String
glFormat (GL es f) = unlines $ ("Total fun: " ++ show f) : sort (map empName es)

processTree :: Tree Employee -> IO ()
processTree = putStrLn . glFormat . maxFun

runTest  :: IO ()
runTest  = processTree testCompany
runTest2 :: IO ()
runTest2 = processTree testCompany2

main :: IO ()
main = readFile filename >>= processTree . read
       where filename = "company.txt"
