{-# LANGUAGE BangPatterns #-}

module Euclid where

{-# INLINE gcd #-}

-- Well known euclidean algorithm.
gcd :: Integral a => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' 0 b = b
    gcd' !a !b = gcd' (b `rem` a) a

{-# INLINE egcd #-}

-- Tail recursive extended euclidean algorithm.
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
