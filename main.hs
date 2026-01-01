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
