module Main where

import qualified Ring (egcd)

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
