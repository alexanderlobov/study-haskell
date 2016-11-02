{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
-- import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n | n < 0 = undefined
      | n == 0 = 1
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s = Cons x (sInterleave s xs)

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = sInterleave (sRepeat n) (ruler' (n + 1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand n = Cons next (rand next)
    where next = (1103515245 * n + 12345) `mod` 2147483648


-- Exercise 8 -----------------------------------------

{- Total Memory in use: 198 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just $ minMaxHelper x x xs

minMaxHelper :: Int -> Int -> [Int] -> (Int, Int)
minMaxHelper minValue maxValue [] = (minValue, maxValue)
minMaxHelper minValue maxValue (x:xs) = minValue `seq` maxValue `seq` minMaxHelper (min minValue x) (max maxValue x) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

-- x11 x12 x21 x22

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
    show (Matrix x11 x12 x21 x22)  = "[ " ++ show x11 ++ " " ++ show x12 ++ "\n"
                           ++ "  " ++ show x21 ++ " " ++ show x22 ++ " ]"

instance Num Matrix where
    (Matrix x11 x12 x21 x22) + (Matrix y11 y12 y21 y22) =
        Matrix (x11 + y11) (x12 + y12)
               (x21 + y21) (x22 + y22)
    (Matrix x11 x12 x21 x22) * (Matrix y11 y12 y21 y22) =
        Matrix (x11 * y11 + x12 * y21) (x11 * y12 + x12 * y22)
               (x21 * y11 + x22 * y21) (x21 * y12 + x22 * y22)
    fromInteger i = Matrix (fromInteger i) 0
                           0 (fromInteger i)


fastFib :: Int -> Integer
fastFib n = case f ^ n of
            Matrix _ x  _ _ -> x

    where f = Matrix 1 1 1 0
