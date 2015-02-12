{-# OPTIONS_GHC -Wall #-}

-- Homework 2

module LogAnalysis where

import Log

-- Exercise 1 - Log file parsing

parseMessage :: String -> LogMessage
parseMessage m = case (words m) of
                    "I":x:s   -> LogMessage Info (read x) (unwords s)
                    "W":x:s   -> LogMessage Warning (read x) (unwords s)
                    "E":x:y:s -> LogMessage (Error (read x)) (read y) (unwords s)
                    s         -> Unknown m

parse :: String -> [LogMessage]
parse s = let ls = lines s
          in map (parseMessage) ls

-- Putting the logs in order

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mTree = mTree
insert logM Leaf         = Node Leaf logM Leaf
insert logM@(LogMessage _ t _) (Node leftT tLogM@(LogMessage _ tlts _) rightT)
    | t <= tlts = Node (insert logM leftT) tLogM rightT
    | otherwise = Node leftT tLogM (insert logM rightT)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                     = []
inOrder (Node Leaf logM Leaf)    = [logM]
inOrder (Node leftT logM rightT) = (inOrder leftT) ++ logM:(inOrder rightT)

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [ s | LogMessage (Error x) _ s <- inOrder (build xs), x >= 50]
