-- Homework 1

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    | x < 0 = []
    | otherwise = let result = x `div` 10
                      remainder = x `mod` 10
                  in toDigits result ++ [remainder]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | even $ length (x:xs) = x * 2 : doubleEveryOther xs
    | otherwise = x : doubleEveryOther xs

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

-- Exercise 4

validate :: Integer -> Bool
validate x = let sumResult = sumDigits $ doubleEveryOther $ toDigits x
             in sumResult `mod` 10 == 0
