{-# LANGUAGE BangPatterns #-}
module Ring where

import Data.STRef.Strict
import Control.Monad.ST.Strict
import Prelude hiding (gcd, lcm)

-- | @'gcd' @a @b returns the greatest 
--   common divisor of two numbers.
gcd :: Integral a => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' 0 b = b
    gcd' !a !b
      | b < a     = gcd' b a
      | otherwise = 
        let !r = b `rem` a              -- r: remainder
        in gcd' (a - r) r

-- | @'lcm' @a @b returns the least
--   common multiple of two numbers.
lcm :: Integral a => a -> a -> a
lcm a 0 = abs a
lcm 0 b = abs b
lcm a b = (a `quot` gcd a b) * b


-- | @'egcd' @a @b returns a valid s and t that solve
--   gcd(a,b) = sa + tb. Meaning it's the extended gcd.
egcd :: Integral a => a -> a -> (a, a)
egcd 0 y = (0, signum y)          -- Filter edge case, to avoid dividing with zero
egcd x y = ((g - tt*yy) `quot` x, signum y * tt)
  where
    yy = abs y
    (tt, g) = go (abs x) yy 0 1        -- tt: our tempT, g: our gcd

    go 0 b _ v = (v, b)                -- v: tempT, b: gcd
    go !a !b !u !v
      | b < a = go b a v u             -- Flip input
      | otherwise = 
      let (q, r) = b `quotRem` a       -- q: quotient, r: remainder
          !uu = u * q
      in go (a - r) r (uu + u - v) (v - uu)


egcdST :: Integral a => a -> a -> (a, a)
egcdST 0 y = (0, signum y)
egcdST x y = runST $ do
  aRef <- newSTRef (abs x)
  bRef <- newSTRef (abs y)
  uRef <- newSTRef 0
  vRef <- newSTRef 1

  go aRef bRef uRef vRef

  myG <- readSTRef bRef
  myT <- readSTRef vRef

  return ((myG - myT * (abs y)) `quot` x, signum y * myT)
  where 
    go a b u v = do 
      a' <- readSTRef a
      if (a' == 0) then do return ()
      else do
        b' <- readSTRef b
        u' <- readSTRef u
        v' <- readSTRef v

        if (b' < a') then do
          writeSTRef a b'
          writeSTRef b a'
          writeSTRef u v'
          writeSTRef v u'
          go a b u v
        else do
          let q = b' `quot` a'
          modifySTRef' b (subtract (q * a'))
          b' <- readSTRef b
          modifySTRef' a (subtract b')

          let t = (q * u')

          modifySTRef' u (+ (t - v'))
          modifySTRef' v (subtract t)
          go a b u v
