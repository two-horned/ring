{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring (gcd)

fib :: Int -> Int
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
main = defaultMain [
  bgroup "gcd" [ bench "Ring, gcd(a,b)"     $ whnf ringcd (a, b)
               , bench "Prelude, gcd(a,b)"  $ whnf pregcd (a, b)
               , bench "Euclid, gcd(a,b)"   $ whnf eucgcd (a, b)
               , bench "Ring, gcd(b,a)"     $ whnf ringcd (b, a)
               , bench "Prelude, gcd(b,a)"  $ whnf pregcd (b, a)
               , bench "Euclid, gcd(b,a)"   $ whnf eucgcd (b, a)
               ]
  ]
  where
    a = fib 1000
    b = fib 999
    pregcd (x, y) = Prelude.gcd x y
    ringcd (x, y) = Ring.gcd x y
    eucgcd (x, y) = euclid x y
