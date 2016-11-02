{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

count :: Eq a => a -> [a] -> Int
count x xs = sum $ map (\y -> if x == y then 1 else 0) xs

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches x y = count True $ zipWith (==) x y

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\y -> count y xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ zipWith min (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- nonexactMatches :: Code -> Code -> Int
-- nonexactMatches secret guess = count True $ map (`elem` secret) guess

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exactCount nonexactCount
    where
        exactCount = exactMatches secret guess
        nonexactCount = (matches secret guess) - exactCount

-- Exercise 4 -----------------------------------------

getMatches :: Move -> (Int, Int)
getMatches (Move _ x y) = (x, y)

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move guess _ _) code =
    getMatches m == getMatches (getMove code guess)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n < 1 = []
    | n == 1 = map (\x -> [x]) colors
    | otherwise = go (allCodes (n-1))
        where
            go codes = concatMap (\code -> map (:code) colors) codes

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = reverse $ step startGuess [] (allCodes (length secret))
    where
        startGuess = [Red, Red, Red, Red]

        step :: Code -> [Move] -> [Code] -> [Move]
        step guess moves codes =
            if (isRight move)
            then move : moves
            else step (newGuess codes') (move:moves) codes'
            where
                move = getMove secret guess
                newGuess xs = head xs
                codes' = filterCodes move codes
                isRight (Move code exactCount _) = length code == exactCount

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
