module Ring where

import Prelude hiding (gcd, lcm)

-- We will first introduce our own implementations
-- of the gcd and the lcd to make it easier for the reader 
-- to understand why our ring function is built the way it is.

-- | @'gcd' @a @b returns the greatest 
--   common divisor of two numbers.
gcd a 0 = abs a
gcd 0 b = abs b
gcd a b = gcd (a - c) c
  where 
    c = b `mod` a

-- | @'lcm' @a @b returns the least
--   common multiple of two numbers.
lcm a 0 = abs a
lcm 0 b = abs b
lcm a b = (a `div` gcd a b) * b


-- For the ring function, 
-- we don't actually need the gcd or lcm functions 
-- from before. 
-- They were only there for demonstrational purposes.

-- | @'ring' @a @b returns a valid s and t that solve
--   gcd(a,b) = sa + tb.
ring :: Integral a => a -> a -> (a,a)
ring a 0 = (signum a, 0)
ring 0 b = (0, signum b)
ring a b = (((s-t)* d + s),(t-s))
  where
    (d,c) = b `divMod` a
    (s,t) = ring (a - c) c
