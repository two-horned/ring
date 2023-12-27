{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Euclid (egcd)
import qualified Ring

leo :: Integral a => a -> a
leo n
  | n < 0 = 0
  | otherwise = go n (-1) 1
  where
    go 0 _ b = b
    go m a b = go (m - 1) b (a + b + 1)

-- Our benchmark harness.
main :: IO ()
main = let
  !third = leo 83 :: Int
  !someleo = leo 77 :: Int

  in defaultMain [ bgroup "egcd/Ring/a=leo(83)" [ bench (unwords ["b =", show someleo]) $ nf (Ring.egcd third) someleo ]
                 , bgroup "egcd/Euclidean/a=leo(83)" [ bench (unwords ["b =", show someleo]) $ nf (Euclid.egcd third) someleo ]
                 ]
