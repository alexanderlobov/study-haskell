{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, (!), (!?), (//))
-- import Data.Vector (cons)
-- import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= \x -> return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV k l v =
    liftM2 (\first second -> v // [(k, second), (l, first)]) (v !? k) (v !? l)


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = foldr (\x -> liftM2 (:) (f x)) (return [])

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices v = mapM (v !?) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = liftM (v !?) $ getRandomR (0, V.length v - 1)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (from, to)= liftM V.fromList $ replicateM n (getRandomR (from, to))

-- Exercise 5 -----------------------------------------


shuffle :: Vector a -> Rnd (Vector a)
shuffle v | V.null v = return v
          | otherwise = iter (V.length v - 1) v
            where iter :: Int -> (Vector a) -> Rnd (Vector a)
                  iter 0 v' = return v'
                  iter i v' = getRandomR (0, i) >>= \j -> iter (i - 1) (swapV' v' i j)
                  swapV' v' i j = v' // [(i, v' ! j), (j, v' ! i)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (V.ifilter (\j el -> el < pivot && j /= i) v,
                   pivot,
                   V.ifilter (\j el -> el >= pivot && j /= i) v)
    where pivot = v ! i

randomPartition :: Ord a => Vector a -> Rnd (Vector a, a, Vector a)
randomPartition v = liftM (partitionAt v) $ getRandomR (0, V.length v - 1)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
    | V.null v = V.empty
    | otherwise = let (lower, pivot, greater) = partitionAt v 0
                  in qsort lower <> V.cons pivot (qsort greater)

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
    | V.null v = return V.empty
    | otherwise = do
        (lower, pivot, greater) <- randomPartition v
        lower' <- qsortR lower
        greater' <- qsortR greater
        return $ lower' <> (V.cons pivot greater')

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select n v
    | n >= V.length v  || n < 0 = return Nothing
    | otherwise = do
        (left, pivot, right) <- randomPartition v
        let leftLength = V.length left
        case compare n leftLength of
            LT -> select n left
            EQ -> return $ Just pivot
            GT -> select (n - leftLength - 1) right

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit | suit <- suits, label <- labels ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
    | V.null deck = Nothing
    | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 deck = Just ([], deck)
getCards n deck
    | V.null deck = Nothing
    | otherwise = do
        (card, deck') <- nextCard deck
        (cards, deck'') <- getCards (n - 1) deck'
        return $ (card:cards, deck'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
