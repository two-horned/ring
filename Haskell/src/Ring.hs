module Ring where

import Prelude hiding (gcd, lcm)

-- We will first introduce our own implementations
-- of the gcd and the lcm to make it easier for the reader 
-- to understand why our ring function is built the way it is.

-- | @'gcd' @a @b returns the greatest 
--   common divisor of two numbers.
gcd :: Integral a => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' 0 b = b
    gcd' a b 
      | b < a     = gcd' b a
      | otherwise = 
        let r = b `rem` a               -- r: remainder
        in gcd' (a - r) r

-- | @'lcm' @a @b returns the least
--   common multiple of two numbers.
lcm :: Integral a => a -> a -> a
lcm a 0 = abs a
lcm 0 b = abs b
lcm a b = (a `quot` gcd a b) * b


-- For the egcd function, 
-- we don't actually need the gcd or lcm functions 
-- from before. 
-- They were only there for demonstrational purposes.

-- | @'egcd' @a @b returns a valid s and t that solve
--   gcd(a,b) = sa + tb. Meaning it's the extended gcd.
egcd :: Integral a => a -> a -> (a,a)
egcd x y = (signum x * s, signum y * t)
  where
    (s, t)    = egcd' (abs x) (abs y)
    egcd' 0 _ = (0, 1)                  -- Always one at base case
    egcd' a b 
      | b < a     = (\(v, u) -> (u, v)) (egcd' b a) -- Flip output
      | otherwise = 
      let (q, r)   = b `quotRem` a      -- q: quotient, r: remainder
          (ss, tt) = egcd' (a - r) r    -- Get intermediate result
      in (((ss - tt) * q + ss), (tt - ss))
