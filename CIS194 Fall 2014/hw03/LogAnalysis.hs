{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Fall 2014: Homework 3

module LogAnalysis where

import Data.List
import Data.Char
import Log

-- Exercise 1 -----------------------------------------

parseMessage :: String -> MaybeLogMessage
parseMessage cs = let wordList = words cs in
    case wordList of
        ("I":t:css)   -> ValidLM $ LogMessage Info (read t) (unwords css)
        ("W":t:css)   -> ValidLM $ LogMessage Warning (read t) (unwords css)
        ("E":e:t:css) -> ValidLM $ LogMessage (Error (read e)) (read t) (unwords css)
        _             -> InvalidLM $ unwords wordList

-- Exercise 2 -----------------------------------------

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM msg : msgs) = msg : validMessagesOnly msgs
validMessagesOnly (_ : msgs) = validMessagesOnly msgs

-- Exercise 3 -----------------------------------------

parse :: String -> [LogMessage]
parse = validMessagesOnly . map parseMessage . lines

-- Exercise 4 -----------------------------------------

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t _) (LogMessage _ t' _) = compare t t'

-- Exercise 5 -----------------------------------------

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- Exercise 6 -----------------------------------------

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error e) _ _) | e >= 50 = True
isRelevant _ = False

mapToStrings :: [LogMessage] -> [String]
mapToStrings = map (\(LogMessage _ _ cs) -> cs)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapToStrings . sortMessages . filter isRelevant

-- Exercise 7 -----------------------------------------

isAbout :: String -> LogMessage -> Bool
isAbout cs (LogMessage _ _ cs') = isInfixOf (map toLower cs) (map toLower cs')

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout = filter . isAbout

-- Exercise 8 -----------------------------------------

(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced cs = mapToStrings . sortMessages . filter (isRelevant ||| isAbout cs)
