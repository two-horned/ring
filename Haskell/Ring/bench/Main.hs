{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring

fib :: Integer -> Integer
fib n 
  | n < 0 = 0
  | otherwise = go n 0 1
  where
    go 0 _ b = b
    go m a b = go (m-1) b (a+b)

euclid :: Integral a => a -> a -> a
euclid x y = euclid' (abs x) (abs y)
  where
    euclid' 0 b = b
    euclid' a b = euclid' (b `rem` a) a

-- Our benchmark harness.
main :: IO ()
main = let 
  !a = fib 109000
  !b = fib 109001
  in defaultMain [ bgroup "gcd" [ bench "Ring, gcd(a,b)"     $ nf (Ring.gcd a) b
                 , bench "Euclid, gcd(a,b)"   $ nf (euclid a) b
                 , bench "Ring, (head)egcd(a,b)"   $ nf (Ring.egcd a) b
                 , bench "Ring, (tail)egcd(a,b)"   $ nf (Ring.tailegcd a) b
                 ]
  ]
