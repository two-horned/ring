{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring

fib :: Integral a => a -> a
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
    euclid' !a !b = euclid' (b `rem` a) a

-- Tail recursive extended euclidean algorithm.
eeuclid :: Integral a => a -> a -> (a, a)
eeuclid x 0 = (signum x, 0)
eeuclid x y = (signum x * ss, (g - xx * ss) `quot` y)
  where 
    xx = abs x
    (ss, g) = go xx (abs y) 1 0
    go 0 b _ s = (s, b)
    go !a !b !t !s =
      let (q, r) = b `quotRem` a
      in go r a (t - q*s) t

-- Our benchmark harness.
main :: IO ()
main = let 
  !a = fib 123455 :: Integer
  !b = fib 123456 :: Integer
  in defaultMain [ bgroup "gcd" [ bench "Ring, gcd(a,b)"     $ nf (Ring.gcd a) b
                 , bench "Euclid, gcd(a,b)"   $ nf (euclid a) b
                 , bench "Ring, egcd(a,b)"   $ nf (Ring.egcd a) b
                 , bench "Euclid, egcd(a,b)"   $ nf (eeuclid a) b
                 , bench "Ring, egcdST(a,b)"   $ nf (Ring.egcdST a) b
                 ]
  ]
