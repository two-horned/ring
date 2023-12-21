module Ring where

-- | @'ring' @a @b returns you a valid s and t that solves
--   gcd(a,b) = sa + tb.

ring :: Integral a => a -> a -> (a,a)
ring a 0 = (signum a, 0)
ring 0 b = (0, signum b)
ring a b = ((t * (d+1) - s*d),(s-t))
  where
    (d,c) = b `divMod` a
    (s,t) = ring c (a-c)
