module Ring where
import Debug.Trace
-- | @'ring' @a @b returns you a valid s that solves
--   gcd(a,b) = sa + tb.
--   
--   If you want to find the corresponding value for t,
--   you simply solve a linear equation.
--
ring :: (Integral a, Show a) => a -> a -> a
ring a b = ring' aa (sig a * sig b)
  where
    sig = signum
    g   = gcd a b
    aa  = ((abs a) `div` g) `mod` bb
    bb  = (abs b) `div` g

    -- Performance.
    ring' a c
      | bb `div` 2 < a = ring'' (bb-a) (-1 * c)
      | otherwise = ring'' a c

    -- The work.
    ring'' 1 c = c
    ring'' a c = trace ("We are at: " ++ (show a)) (ring'' (go a) (go' a c))
    go a = a - (bb `mod` a)
    go' a c = (c * (bb `div` a + 1)) `mod` bb
