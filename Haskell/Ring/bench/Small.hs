{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring

leo :: Integral a => a -> a
leo n
  | n < 0 = 0
  | otherwise = go n (-1) 1
  where
    go 0 _ b = b
    go m a b = go (m - 1) b (a + b + 1)

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
  !third = leo 83 :: Int
  !someleo = leo 77 :: Int

  in defaultMain [ bgroup "egcd/Ring/a=leo(83)" [ bench (unwords ["b =", show someleo]) $ nf (Ring.egcd third) someleo ]
                 , bgroup "egcd/Euclidean/a=leo(83)" [ bench (unwords ["b =", show someleo]) $ nf (eeuclid third) someleo ]
                 ]
