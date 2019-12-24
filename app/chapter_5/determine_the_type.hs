module DetermineTheType where

example :: Integer
example = 1

-- (* 9) 6 -- 54 :: Num a => a
-- head [(0, "doge"), (1, "kitten")] -- (0, "doge") :: Num a => (a, [Char])
-- if False then True else False :: Bool
-- length [1..5] :: Int
-- ( length [1..5] ) > (length "TACOCAT") :: Bool

functionH :: [a] -> a
functionH (x : _) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y ) then True else False

functionS :: (a, b) -> b
functionS (_, y) = y
