module Golf where

-- Homework 3

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = map (nthElems xs) [1..length xs]

nthElems :: [a] -> Int -> [a]
nthElems xs p = case drop (p-1) xs of
                    [] -> []
                    (y:ys) -> y : nthElems ys p
