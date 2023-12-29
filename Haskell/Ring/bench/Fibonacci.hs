{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import qualified Ring
import qualified Euclid

maxTest :: Int
maxTest = 91

fib :: Integral a => a -> a
fib n 
  | n < 0 = 0
  | otherwise = go n 0 1
  where
    go 0 _ b = b
    go m a b = go (m - 1) b (a+b)

-- Our benchmark harness.
main :: IO ()
main = let 
  !first = fib 21 :: Int
  !second = fib 57 :: Int
  !third = fib 91 :: Int 
  !testList = [ fib x | x <- [0..maxTest] ]

  in defaultMain 
    [ bgroup "gcd"
      [ bgroup (show first)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd first) b | b <- testList ]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd first) b | b <- testList]
        ]
      , bgroup (show second)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd second) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd second) b | b <- testList]
        ]
      , bgroup (show third)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.gcd third) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.gcd third) b | b <- testList]
        ]
      ]
    , bgroup "egcd"
      [ bgroup (show first)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd first) b | b <- testList ]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd first) b | b <- testList]
        ]
      , bgroup (show second)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd second) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd second) b | b <- testList]
        ]
      , bgroup (show third)
        [ bgroup "Ring" [ bench (unwords ["b =", show b]) $ nf (Ring.egcd third) b | b <- testList]
        , bgroup "Euclidean" [ bench (unwords ["b =", show b]) $ nf (Euclid.egcd third) b | b <- testList]
        ]
      ]
    ]
