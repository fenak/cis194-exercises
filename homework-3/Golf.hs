module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs = xs : map (nthElems xs) [2..length xs]

nthElems :: [a] -> Int -> [a]
nthElems xs p = case drop (p-1) xs of
                    [] -> []
                    (y:ys) -> y : nthElems ys p
