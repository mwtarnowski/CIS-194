{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2015: Homework 3

module HW03 where

data Expression =
      Var String                   -- Variable
    | Val Int                      -- Integer literal
    | Op Expression Bop Expression -- Operation
    deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
      Plus     
    | Minus    
    | Times    
    | Divide   
    | Gt
    | Ge       
    | Lt  
    | Le
    | Eql
    deriving (Show, Eq)

data Statement =
      Assign   String     Expression
    | Incr     String
    | If       Expression Statement  Statement
    | While    Expression Statement       
    | For      Statement  Expression Statement Statement
    | Sequence Statement  Statement        
    | Skip
    deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s cs n cs' | cs == cs' = n
                  | otherwise = s cs'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

evalE :: State -> Expression -> Int
evalE _ (Val n) = n
evalE s (Var cs) = s cs
evalE s (Op expr1 op expr2) = case op of
    Plus   -> n1 + n2
    Minus  -> n1 - n2
    Times  -> n1 * n2
    Divide -> n1 `div` n2
    Gt     -> boolToInt $ n1 >  n2
    Ge     -> boolToInt $ n1 >= n2
    Lt     -> boolToInt $ n1 <  n2
    Le     -> boolToInt $ n1 <= n2
    Eql    -> boolToInt $ n1 == n2
    where n1 = evalE s expr1
          n2 = evalE s expr2

-- Exercise 3 -----------------------------------------

data DietStatement = 
      DAssign String Expression
    | DIf Expression DietStatement DietStatement
    | DWhile Expression DietStatement
    | DSequence DietStatement DietStatement
    | DSkip
    deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign cs expr) = DAssign cs expr
desugar (Incr cs) =  DAssign cs (Op (Var cs) Plus (Val 1))
desugar (If expr st1 st2) = DIf expr (desugar st1) (desugar st2)
desugar (While expr st) = DWhile expr (desugar st)
desugar (For st1 expr st2 st3) = DSequence (desugar st1) (DWhile expr (DSequence (desugar st3) (desugar st2)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign cs expr) 
                       = extend s cs (evalE s expr)
evalSimple s (DIf expr st1 st2) 
    | evalE s expr > 0 = evalSimple s st1 
    | otherwise        = evalSimple s st2
evalSimple s stw@(DWhile expr st)
    | evalE s expr > 0 = evalSimple s (DSequence st stw)
    | otherwise        = evalSimple s DSkip
evalSimple s (DSequence st1 st2) 
                       = evalSimple (evalSimple s st1) st2
evalSimple s DSkip     = s

run :: State -> Statement -> State
run s st = evalSimple s $ desugar st

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
