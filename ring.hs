module Ring where
import Debug.Trace
-- | @'ring' @a @b returns you a valid s that solves
--   gcd(a,b) = sa + tb, where b is prime.
--   
--   If you want to find the corresponding value for t,
--   you simply solve a linear equation.
--
ring :: (Integral a, Show a) => a -> a -> a
ring a b = ring' aa bb cc
  where
    aa  = (abs a) `mod` bb
    bb  = abs b
    cc  = signum a * signum b

    ring' 1 _ c = c
    ring' a b c 
      | b `div` 2 < a = ring' (b-a) b (-c)
      | otherwise = trace ("Trace: " ++ (show a) ++ ", " ++ show(b) ++ ", " ++ show (c)) 
      (ring' (b `mod` a) b (go a b c))
    go a b c = (-c) * (b `div` a)
