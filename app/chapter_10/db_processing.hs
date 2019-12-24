module DatabaseProcessing where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Show, Eq, Ord)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 14
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbs = [date | DbDate date <- dbs]
-- filterDbDate dbs = foldl (filterAndMapDate) [] dbs
--   where filterAndMapDate saved (DbDate date) = date : saved
--         filterAndMapDate ds _ = ds

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbs = [ num | DbNumber num <- dbs ]
-- filterDbNumber dbs = foldl (filterAndMapNum) [] dbs
--   where filterAndMapNum saved (DbNumber num) = num : saved
--         filterAndMapNum ds _ = ds

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate
-- mostRecent (time : []) = time
-- mostRecent (x : xs) = foldl (\x y -> x > y) x . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = (\(a, b) -> (fromIntegral a::Double) / (fromIntegral b::Double))
  . average
  . filterDbNumber

average :: Num a => [a] -> (a,a)
average as = foldl (\x y -> (y + fst x, 1 + snd x)) (0, 1) as
