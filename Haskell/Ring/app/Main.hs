module Main where

import qualified Ring (egcd)
import Control.Monad.ST
import Data.STRef

fibST :: Integer -> Integer
fibST n = 
    if n < 2
    then n
    else runST $ do
        x <- newSTRef 0
        y <- newSTRef 1
        fibST' n x y

    where fibST' 0 x _ = readSTRef x
          fibST' n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y $! x'+y'
              fibST' (n-1) x y

fib :: Integer -> Integer
fib n 
  | n < 0 = 0
  | otherwise = go n 0 1
  where
    go 0 _ b = b
    go m a b = go (m-1) b (a+b)

main :: IO ()
main = do
  putStrLn $ unwords ["EGCD of", show a, "and", show b, "is", show (Ring.egcd a b)]
  where 
    a = fib 100 
    b = fib 101
