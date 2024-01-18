said :: Int -> Int
said 0 = 0
said 1 = 1
said n
  | n `rem` 2 == 0 = said (n-1) + said (n-2)
  | otherwise      = said (n-1) + said (n-3)


ssaid n = go n 0 1 0
  where
    go 0 a _ _ = a
    go n a b c = go (n-1) b (b+c) (b+a)
