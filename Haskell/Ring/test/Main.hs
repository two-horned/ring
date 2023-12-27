module Main (main) where

import qualified Ring (egcd, gcd)
import System.Exit (exitSuccess, exitFailure)
import Debug.Trace (trace)

myRange :: [Integer]
myRange = [-1000..1000]

fromTuple :: Integral a => a -> a -> (a, a) -> a
fromTuple a b (s, t) = s * a + t * b

main :: IO ()
main = do
  ptm $ and equallist
  where
    cmpgcds x y = trace ("at: x=" ++ show x ++ " and y=" ++ show y) 
                        ((Ring.gcd x y) == fromTuple x y (Ring.egcd x y))
    equallist = [ cmpgcds x y | x <- myRange, y <- myRange ]
    ptm x = if x then exitSuccess else exitFailure
