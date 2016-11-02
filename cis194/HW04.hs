{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
 (P a) == (P a') = a == a'

-- Exercise 3 -----------------------------------------


instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P c) = merge $ showTerms c 0
        where
            showTerms :: (Num a, Eq a, Show a) => [a] -> Int -> [String]
            showTerms (0:ys) counter = showTerms ys (counter + 1)
            showTerms (y:ys) counter = showTerm y counter : showTerms ys (counter + 1)
            showTerms [] _ = []

            showTerm :: (Num a, Show a, Eq a) => a -> Int -> String
            showTerm coef 0 = show coef
            showTerm coef 1 = showCoef coef ++ "x"
            showTerm coef expCounter = showCoef coef ++ "x^" ++ show expCounter

            merge :: [String] -> String
            merge (y:[]) = y
            merge (y:ys) = merge ys ++ " + " ++ y
            merge [] = ""

            showCoef :: (Num a, Eq a, Show a) => a -> String
            showCoef coef
                | coef == 1 = ""
                | coef == -1 = "-"
                | otherwise = show coef

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P y) (P z) = P $ zipCoefs y z
    where zipCoefs (t:ts) (p:ps) = (t + p) : zipCoefs ts ps
          zipCoefs [] (p:ps) = p : zipCoefs [] ps
          zipCoefs (t:ts) [] = t : zipCoefs ts []
          zipCoefs [] [] = []

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P y) (P z) = sum $ timesList y z 0
    where timesList :: Num a => [a] -> [a] -> Int -> [Poly a]
          timesList [] _ _ = []
          timesList (t:ts) p counter = (P $ shift counter $ map (* t) p)
                                       : timesList ts p (counter + 1)
          shift counter l = (replicate counter 0) ++ l

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P (map negate l)
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) value = helper l 1
    where helper (y:ys) v = y * v + helper ys (v * value)
          helper [] _ = 0

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 y = deriv y
    nderiv n y = nderiv (n - 1) (deriv y)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P l) = P (polyDeriv l 0)

polyDeriv :: Num a => [a] -> Int -> [a]
polyDeriv [] _ = []
polyDeriv (_:ys) 0 = polyDeriv ys 1
polyDeriv (y:ys) degree = y * (fromIntegral degree) : polyDeriv ys (degree + 1)

