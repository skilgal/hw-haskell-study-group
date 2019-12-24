module ChapterExercises where

import Data.Char

onlyUpper :: String -> String
onlyUpper = filter isUpper

cap :: String -> String
cap (h : t) = [toUpper h] ++ t
cap [] = []

myOr :: [Bool] -> Bool
myOr (x : xs) = x || myOr(xs)
myOr [] = True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x : xs) = f x && myAny f xs
myAny _ [] = True

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f xs (head xs) where
  go _ [] max = max
  go f (x : xs) max =
    go f xs (if f x max == GT then x else max)


myElem :: Eq a => a -> [a] -> Bool
myElem a (x : xs) = if a == x then True else myElem a xs
myElem _ [] = False


myReverse :: [a] -> [a]
myReverse (x : xs) = myReverse xs ++ [x]
myReverse [] = []


squish :: [[a]] -> [a]
squish (h : t) = h ++ squish t
squish [] = []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f (x : xs) = f x ++ squishMap f xs
squishMap _ [] = []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
