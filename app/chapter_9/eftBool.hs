module EftBool where

eftCommon :: (Eq a, Ord a) => a -> a -> [a]
eftCommon next f s
  | f > s = []
  | f == s = [s]
  | otherwise = f : eftCommon (next f) s


    -- succ True throws exeption. Need to check
eftBool :: Bool -> Bool -> [Bool]
eftBool = eftCommon (\x -> if x == False then True)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftCommon

eftInt :: Int -> Int -> [Int]
eftInt = eftCommon _

eftChar :: Char -> Char -> String
eftChar = eftCommon _
