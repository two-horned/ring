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

-- Well known euclidean algorithm.
euclid :: Integral a => a -> a -> a
euclid x y = euclid' (abs x) (abs y)
  where
    euclid' 0 b = b
    euclid' a b = euclid' (b `rem` a) a

-- Head recursive extended euclidean algorithm.
eeuclid :: Integral a => a -> a -> (a, a)
eeuclid x y = (signum x * s, signum y * t)
  where 
    (s, t) = eeuclid' (abs x) (abs y)
    eeuclid' 0 b = (0, signum b)
    eeuclid' a b = 
      let (ss, tt) = eeuclid' r a
          (q, r) = b `quotRem` a
      in (tt - q*ss, ss)

-- Head recursive extended euclidean algorithm.
taileeuclid :: Integral a => a -> a -> (a, a)
taileeuclid x 0 = (signum x, 0)
taileeuclid x y = (signum x * ss, (g - xx * ss) `quot` y)
  where 
    xx = abs x
    (ss, g) = go xx (abs y) 1 0
    go 0 b _ s = (s, b)
    go a b t s =
      let (q, r) = b `quotRem` a
      in go r a (t - q*s) t

-- Our benchmark harness.
main :: IO ()
main = let 
  !a = fib 109000
  !b = fib 109001
  in defaultMain [ bgroup "gcd" [ bench "Ring, gcd(a,b)"     $ nf (Ring.gcd a) b
                 , bench "Euclid, gcd(a,b)"   $ nf (euclid a) b
                 , bench "Ring, (head)egcd(a,b)"   $ nf (Ring.egcd a) b
                 , bench "Euclid, (head)egcd(a,b)"   $ nf (eeuclid a) b
                 , bench "Ring, (tail)egcd(a,b)"   $ nf (Ring.tailegcd a) b
                 , bench "Euclid, (tail)egcd(a,b)"   $ nf (taileeuclid a) b
                 ]
  ]
