{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring
import qualified Euclid

maxTest :: Int
maxTest = 89

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
  !first = leo 21 :: Int
  !second = leo 57 :: Int
  !third = leo 83 :: Int 
  !testList = [ leo x | x <- [0..maxTest] ]

  in defaultMain 
    [ bgroup "gcd"
      [ bgroup "leo(21)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd first) b | b <- testList ]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd first) b | b <- testList]
        ]
      , bgroup "leo(57)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd second) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd second) b | b <- testList]
        ]
      , bgroup "leo(83)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd third) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd third) b | b <- testList]
        ]
      ]
    , bgroup "egcd"
      [ bgroup "leo(21)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd first) b | b <- testList ]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd first) b | b <- testList]
        ]
      , bgroup "leo(57)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd second) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd second) b | b <- testList]
        ]
      , bgroup "leo(83)"
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd third) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd third) b | b <- testList]
        ]
      ]
    ]
