module Golf where

import Data.List

-- Homework 3

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = map (nthElems xs) [1..length xs]

nthElems :: [a] -> Int -> [a]
nthElems xs p = case drop (p-1) xs of
                    [] -> []
                    (y:ys) -> y : nthElems ys p

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (w:x:y:zs)
    | w < x && x > y = x : localMaxima (x:y:zs)
    | otherwise      = localMaxima (x:y:zs)
localMaxima _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = intercalate "\n" $ (reverse . hLines . countIntL) xs ++ ["==========", "0123456789"]

hLines :: [Integer] -> [String]
hLines [] = []
hLines xs = case any (>0) xs of
    True  -> mapLine xs : (hLines $ decrL xs)
    False -> []

mapLine :: [Integer] -> String
mapLine = map (\x -> if x > 0 then '*' else ' ')

countIntL :: [Integer] -> [Integer]
countIntL xs = map (count xs) [0..9]

count :: [Integer] -> Int -> Integer
count xs x = toInteger $ length $ filter (\y -> (toInteger x) == y) xs

decrL :: [Integer] -> [Integer]
decrL = map (\x -> if x > 0 then x-1 else x)
