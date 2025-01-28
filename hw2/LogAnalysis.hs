{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.Read (readMaybe)

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : ts : msg)
    | Just timestamp <- readMaybe ts ->
        LogMessage Info timestamp (unwords msg)
  ("W" : ts : msg)
    | Just timestamp <- readMaybe ts ->
        LogMessage Warning timestamp (unwords msg)
  ("E" : err : ts : msg)
    | Just timestamp <- readMaybe ts,
      Just errCode <- readMaybe err ->
        LogMessage (Error errCode) timestamp (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse xs = parseMessage <$> lines xs

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ t1 _) (Node lt m2@(LogMessage _ t2 _) rt)
  | t1 < t2 = Node (insert m1 lt) m2 rt
  | otherwise = Node lt m2 (insert m1 rt)
insert _ t = t

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap toMessage . filter isSevereError . inOrder . build

toMessage :: LogMessage -> String
toMessage (Unknown m) = m
toMessage (LogMessage _ _ m) = m

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _ = False
