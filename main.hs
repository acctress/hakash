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

-- Another example of using recursion in Haskell.
-- A simple string equation checker.
areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True                                        -- Two empty lists (empty strings) are equal
areStringsEq (x : xs) (y : ys) = x == y && areStringsEq xs ys    -- Compare the first two characters, and then call areStringEqual to compare the rest of the string.
areStringsEq _ _ = False                                         -- Any garbage inputted will return false

-- A simple lambda example
dbl1to10 = map (\x -> x * 2) [1..10]
-- This can also be done this way to avoid lambda for something so simple
dbl1to10 = map (* 2) [1..10]

-- Switch case example
getClass :: Int -> String
getClass n = case n of
  5 -> "go to kindergarten"
  6 -> "go to elementary"
  _ -> "go away"

-- Enums
data BaseballPlayer
  = Pitcher
  | Catcher
  | Infielder
  | Outfield
  deriving (Show) -- This allows us to print an enumeration

player :: BaseballPlayer -> Bool
player Outfield = True

playerIsOF = print (player Outfield) -- True

-- A custom data type, almost like a class.
data Customer = Customer String String Double
  deriving (Show)

bobSmith :: Customer
bobSmith = Customer "Bob Smith" "123 Street" 20.50

-- This will return the Double of the customer, which is the Customers' balance
getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b -- This only cares about the Double field, which is the balance

-- ghci> getBalance bobSmith
-- 20.5

-- An enum to represent different play states in rock paper scissors.
data RPS = Rock | Paper | Scissors

-- Pattern matching for each state
shoot :: RPS -> RPS -> String
shoot Paper Rock = "paper beats rock"
shoot Rock Scissors = "rock beats scissors"
shoot Scissors Paper = "scissors beats paper"
shoot Scissors Rock = "scissors loses to rock"
shoot Paper Scissors = "paper loses to scissors"
shoot Rock Paper = "rock loses to paper"
shoot _ _ = "what are you doing"

-- Two versions of a type.
data Shape
  = Circle Float Float Float -- x, y, radius
  | Rectangle Float Float Float Float -- top left, top right, etc
  deriving (Show)

area :: Shape -> Float
area (Circle _ _ radius) = pi * radius ^ 2
area (Rectangle x y x2 y2) = abs (x2 - x) * abs (y2 - y)

--                we can do: (abs $ x2 - x) * (abs $ y2 - y)
--                to avoid using parenthesis, but the approach chosen is cleaner

flatPlane :: Shape
flatPlane = Rectangle 0.0 10.0 5.0 10.0

-- Dot operator
-- allows us to chain functions to pass output from the right to input on the left
sumValue = putStrLn . show $ 1 + 2
-- We can just use print here

-- Type classes.
data Employee = Employee
  { name :: String,
    position :: String,
    idNum :: Int
  }
  deriving (Eq, Show)

-- We derived as Eq and Show because we want to be able to
-- show this data type as a string but also do equality on it

bobSmith = Employee {name = "Bob Smith", position = "Manager", idNum = 1000}
johnDoe = Employee {name = "John Doe", position = "Sales", idNum = 1001}

isBobJohn = bobSmith == johnDoe
bobSmithData = show bobSmith

-- Another type class
data ShirtSize = S | M | L

-- Actually define the type instances and how they will work with Eq
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

-- Do the same for Show
instance Show ShirtSize where
  show S = "small"
  show M = "medium"
  show L = "large"

-- Is it in a list?
smallAvailable = S `elem` [S, M, L]
theSize = show S

-- A real class to check for equality.
-- a represents any type that will implement MyEq
class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newNewShirtSize = areEqual M M

-- Simple IO function
greet = do
  putStr "hi what's ur name? "
  name <- getLine
  putStrLn $ "hi " ++ name ++ "!!"

-- Write to file function
writeToFile :: FilePath -> String -> IO ()
writeToFile filename txt = do
  file <- openFile filename WriteMode
  hPutStr file txt
  hClose file

-- Read a file and print it's content
printFile :: FilePath -> IO ()
printFile filename = do
  file <- openFile filename ReadMode
  content <- hGetContents file
  putStrLn content
  hClose file
