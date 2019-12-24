module FearfulSymmetry where

myWords :: String -> [String]
myWords [] = []
myWords s = takeWord s : (myWords . dropWord) s
  where dropWord = dropWhile (==' ') . dropWhile (/=' ')
        takeWord = takeWhile (/=' ')
