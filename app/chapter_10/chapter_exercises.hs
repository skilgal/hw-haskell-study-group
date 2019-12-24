module Chapter10 where

import Data.List.NonEmpty hiding (filter)

tuples :: [a] -> [a] -> [(a, a, a)]
tuples a b = [(x, y, z) | x <- a, y <- b, z <- a]

tuplesPStart :: Eq a => [a] -> [a] -> a -> [(a, a, a)]
tuplesPStart a b c = filter (startsWith c) (tuples a b)
  where
    startsWith p (start, _, _)
      | p == start = True
      | otherwise = False

-- count the average size of the word
-- seekritFunc x = div (sum (map length (words x))) (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x y -> p x && y) True

-- Prelude> myAny even [1, 3, 5]
-- False
-- Prelude> myAny odd [1, 3, 5]
-- True

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\x y -> x == el || y) False
-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []
     -- Prelude> myReverse "blah"
     -- "halb"
     -- Prelude> myReverse [1..5]
     -- [5,4,3,2,1]

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then [x] else [] ++ y) []

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldl (\x y -> x ++ f y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
myMaximumBy f (h :| t) = foldl (\x y -> if f x y == GT then x else y) h t

-- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- 10
-- Prelude> myMaximumBy compare [1..10]
-- 10


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (h:t) = foldl (\x y -> if f x y == LT then x else y) h t

-- Prelude> myMinimumBy (\_ _ -> GT) [1..10]
-- 10
-- Prelude> myMinimumBy (\_ _ -> LT) [1..10]
-- 1
-- Prelude> myMinimumBy compare [1..10]
-- 1
