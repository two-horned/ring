{-# LANGUAGE BangPatterns #-}

module Euclid where

import Prelude hiding (gcd, lcm)

{-# INLINE gcd #-}

-- | @'gcd' @a @b returns the greatest 
--   common divisor of two numbers.
gcd :: Integral a => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' 0 b = b
    gcd' !a !b = gcd' (b `rem` a) a

{-# INLINE lcm #-}

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
egcd x 0 = (signum x, 0)
egcd !x !y = (signum x * ss, (g - xx * ss) `quot` y)
  where 
    !xx = abs x
    (ss, g) = go xx (abs y) 1 0
    go 0 b _ s = (s, b)
    go !a !b !t !s =
      let (!q, !r) = b `quotRem` a
      in go r a (t - q*s) t
