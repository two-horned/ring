{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring

maxTest :: Int
maxTest = 89

leo :: Integral a => a -> a
leo n 
  | n < 0 = 0
  | otherwise = go n (-1) 1
  where
    go 0 _ b = b
    go m a b = go (m - 1) b (a + b + 1)

{-# INLINE euclid #-}
-- Well known euclidean algorithm.
euclid :: Integral a => a -> a -> a
euclid x y = euclid' (abs x) (abs y)
  where
    euclid' 0 b = b
    euclid' !a !b = euclid' (b `rem` a) a

{-# INLINE eeuclid #-}
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
  !first = leo 21 :: Int
  !second = leo 57 :: Int
  !third = leo 83 :: Int 
  !testList = [ leo x | x <- [0..maxTest] ]

  in defaultMain [ bgroup "gcd/Ring/a=leo(21)" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd first) b | b <- testList ]
                 , bgroup "gcd/Ring/a=leo(57)" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd second) b | b <- testList]
                 , bgroup "gcd/Ring/a=leo(83)" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd third) b | b <- testList]
                 -- Euclidean now.
                 , bgroup "gcd/Euclidean/a=leo(21)" [ bench (unwords ["b =", show b]) $ nf (euclid first) b | b <- testList]
                 , bgroup "gcd/Euclidean/a=leo(57)" [ bench (unwords ["b =", show b]) $ nf (euclid second) b | b <- testList]
                 , bgroup "gcd/Euclidean/a=leo(83)" [ bench (unwords ["b =", show b]) $ nf (euclid third) b | b <- testList]
                 -- Extended now.
                 , bgroup "egcd/Ring/a=leo(21)" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd first) b | b <- testList]
                 , bgroup "egcd/Ring/a=leo(57)" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd second) b | b <- testList]
                 , bgroup "egcd/Ring/a=leo(83)" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd third) b | b <- testList]
                 -- Extended Euclidean now.
                 , bgroup "egcd/Euclidean/a=leo(21)" [ bench (unwords ["b =", show b]) $ nf (eeuclid first) b | b <- testList]
                 , bgroup "egcd/Euclidean/a=leo(57)" [ bench (unwords ["b =", show b]) $ nf (eeuclid second) b | b <- testList]
                 , bgroup "egcd/Euclidean/a=leo(83)" [ bench (unwords ["b =", show b]) $ nf (eeuclid third) b | b <- testList]
                 ]
