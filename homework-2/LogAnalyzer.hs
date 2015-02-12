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
                    s         -> Unknown (unwords s)

parse :: String -> [LogMessage]
parse s = let ls = lines s
          in map (parseMessage) ls
