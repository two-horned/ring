{-# LANGUAGE BangPatterns #-}

module Ring where

import Prelude hiding (gcd, lcm)

{-# INLINE gcd #-}

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


{-# INLINE egcd #-}

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
