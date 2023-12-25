module Main (main) where

import qualified Ring (egcd, tailegcd)
import System.Exit (exitSuccess, exitFailure)
import Debug.Trace (trace)

myRange :: [Integer]
myRange = [-1000..1000]

main :: IO ()
main = do
  ptm $ and equallist
  where
    cmpgcds x y = trace ("at: x=" ++ show x ++ " and y=" ++ show y) 
                        ((Ring.egcd x y) == (Ring.tailegcd x y))
    equallist = [ cmpgcds x y | x <- myRange, y <- myRange ]
    ptm x = if x then exitSuccess else exitFailure
