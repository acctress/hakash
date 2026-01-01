import Data.List

-- A simple function to check if a character is an alphabetical character
isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

-- For every number which is less than 10, replace with BOOM!, else BANG! - if it isn't a number get rid of it.
boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- A custom function to retrieve the length of a list
length' :: (Num a) => [t] -> a
length' xs = sum [1 | _ <- xs]

-- Returns a list of tuples which represent triangles
triangles :: [(Integer, Integer, Integer)]
triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

-- Create a higher order function which will map each number in a list to itself times four.
times4 :: Int -> Int
times4 x = x * 4

-- Map a list of numbers starting from one to five using the times4 function.
listTimes4 = map times4 [1 .. 5]
-- [4,8,12,16,20]

-- A simple implementation of a map function
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x : xs) = times4 x : multBy4 xs
-- x is the first item, xs is the rest of the items
