module Main where

import Scheme
import System.Environment (getArgs)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

