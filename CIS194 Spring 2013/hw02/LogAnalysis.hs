{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2013: Homework 2

module LogAnalysis where

import Log

-- Exercise 1 -----------------------------------------

parseMessage :: String -> LogMessage
parseMessage cs = let wordList = words cs in
    case wordList of
        ("I":t:css)   -> LogMessage Info (read t) (unwords css)
        ("W":t:css)   -> LogMessage Warning (read t) (unwords css)
        ("E":e:t:css) -> LogMessage (Error (read e)) (read t) (unwords css)
        _             -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2 -----------------------------------------

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t _) (Node left msg'@(LogMessage _ t' _) right)
    | t <= t'   = Node (insert msg left) msg' right
    | otherwise = Node left msg' (insert msg right)
insert _ tree = tree

-- Exercise 3 -----------------------------------------

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgs) = insert msg (build msgs)

build' :: [LogMessage] -> MessageTree
build' = foldr insert Leaf

-- Exercise 4 -----------------------------------------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5 -----------------------------------------

whatWentWrongRaw :: [LogMessage] -> [String]
whatWentWrongRaw [] = []
whatWentWrongRaw ((LogMessage (Error e) _ txt):msgs)
    | e >= 50   = txt : whatWentWrongRaw msgs
    | otherwise =       whatWentWrongRaw msgs
whatWentWrongRaw (_:msgs) = whatWentWrongRaw msgs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = whatWentWrongRaw . inOrder . build

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' = mapToStrings . inOrder . build . filter isRelevant
                 where isRelevant (LogMessage (Error e) _ _) | e >= 50 = True
                       isRelevant _ = False
                       mapToStrings = map (\(LogMessage _ _ cs) -> cs)
