{-# OPTIONS_GHC -Wall #-}
-- Type-Driven Development Exercises
-- Source: http://tonymorris.github.io/blog/20-intermediate-haskell-exercises/ 

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f (x:xs) = f x : furry f xs
  furry _ _ = []

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just x) = Just $ f x
  furry _ _ = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft  b a = EitherLeft  (Either a b) deriving Show
newtype EitherRight a b = EitherRight (Either a b) deriving Show

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x))  = EitherLeft . Left  $ f x
  furry _ (EitherLeft (Right x)) = EitherLeft . Right $ x

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left x))  = EitherRight . Left  $ x
  furry f (EitherRight (Right x)) = EitherRight . Right $ f x

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana $ unicorn . f

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f (x:xs) = f x ++ banana f xs
  banana _ _ = []
  unicorn = (:[])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just x) = f x
  banana _ _  = Nothing
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g = \t -> f (g t) t
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x))  = f x
  banana _ (EitherLeft (Right x)) = EitherLeft . Right $ x
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  banana _ (EitherRight (Left x))  = EitherRight . Left $ x
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = banana . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy (x:xs) f = apple (moppy xs f) (furry' (:) (f x))
moppy _ _ = unicorn []

-- Exercise 15
-- Relative Difficulty: 6
sausage :: (Misty m) => [m a] -> m [a]
sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = flip apple `compose` furry'
          where compose = (.).(.)

-- Exercise 17
-- Relative Difficulty: 6
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = flip apple `compose` banana2
          where compose = (.).(.).(.)

-- Exercise 18
-- Relative Difficulty: 6
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = flip apple `compose` banana3
          where compose = (.).(.).(.).(.)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f st = State (\s -> mapSnd f $ state st s)
               where mapSnd f' (x, y) = (x, f' y)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f st = State (\s -> let (s', x) = state st s in state (f x) s')
  unicorn x   = State (\s -> (s, x))

