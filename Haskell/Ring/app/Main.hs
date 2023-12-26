module Main where

import qualified Ring (egcd)
import Control.Monad.ST
import Data.STRef

fibST :: Integer -> Integer
fibST n = 
    if n < 2
    then n
    else runST $ do
        x <- newSTRef 0
        y <- newSTRef 1
        fibST' n x y

    where fibST' 0 x _ = readSTRef x
          fibST' n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y $! x'+y'
              fibST' (n-1) x y

fib :: Integer -> Integer
fib n 
  | n < 0 = 0
  | otherwise = go n 0 1
  where
    go 0 _ b = b
    go m a b = go (m-1) b (a+b)

main :: IO ()
main = do
  putStrLn $ unwords ["EGCD of", show a, "and", show b, "is", show (Ring.egcd a b)]
  where 
    a = fib 100 
    b = fib 101


-- | @'egcd' @a @b returns a valid s and t that solve
--   gcd(a,b) = sa + tb. Meaning it's the extended gcd.
--egcd :: (Show a, Integral a, Num a) => a -> a -> (a, a)
--egcd 0 y = (0, signum y)          -- Filter edge case, to avoid dividing with zero
--egcd x y = runST $ do 
--x' <- newSTRef (abs x)
--y' <- newSTRef (abs y)
--s' <- newSTRef (x)
--t' <- newSTRef (y)

--return (go x' y' s' t')
--where
--  go a b u v = do { a' <- readSTRef a
--                  ; b' <- readSTRef b
--                  ; u' <- readSTRef u
--                  ; v' <- readSTRef v
--                  ; (v', b')
--  }
--    --; if b' < a' then go b a v u
--    --else
--      --case (a', b', u', v') of
--        --(0, gg, _, tt) -> (tt, gg)
--        --_ -> 
--          --let (q, r) = b' `quotRem` a'
--              --uu = u' * q
--          --in 
--            --writeSTRef a (a' - r)
--            --writeSTRef b r
--            --modifySTRef u (\l -> l + (uu - v'))
--            --modifySTRef v (\l -> l - uu)
--            --go a b u v
