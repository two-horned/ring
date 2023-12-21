module Ring where

-- | @'gcd' @a @b returns the greatest 
--   common divisor of two numbers.
gcd a 0 = abs a
gcd 0 b = abs b
gcd a b = Ring.gcd c (a-c)
  where 
    c = b `mod` a

-- | @'ring' @a @b returns a valid s and t that solves
--   gcd(a,b) = sa + tb.
ring :: Integral a => a -> a -> (a,a)
ring a 0 = (signum a, 0)
ring 0 b = (0, signum b)
ring a b = ((t * (d+1) - s*d),(s-t))
  where
    (d,c) = b `divMod` a
    (s,t) = ring c (a-c)
