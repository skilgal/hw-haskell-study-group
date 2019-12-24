module Summ where

sumO :: (Num a, Eq a) => a -> a
sumO num = go num 0 0
  where go n c s
          | n == 0 = s
          | otherwise = 1 + go (n - 1) (c + 1) (s + c)


multiply :: (Integral a) => a -> a -> a
multiply a b = go a b 1
  where go base summary counter
          | counter == base = summary
          | otherwise = go base (summary + base) (counter +1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero

dividedByM :: Integral a => a -> a -> (DividedResult, a)
dividedByM num denom = go num denom 0
  where go n d count
          | d == 0 = (DividedByZero, d)
          | n < d = (Result count, n)
          | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) (n + 11)
