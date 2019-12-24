module WordNumber where
import Data.List (intersperse, intercalate)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = error "There is a number, not a digit. Digit range [0..9]"

digits :: Int -> [Int]
digits n
  | n < 9 = [n]
  | otherwise = digits (n `div` 10 ) ++ [n `mod` 10]

wordNumber :: Int -> String
-- wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
-- wordNumber n = (concat . intersperse "-" . map digitToWord . digits) n
-- wordNumber n = intercalate "-" (map digitToWord (digits n))
-- wordNumber n = intercalate "-" (map digitToWord (digits n))
wordNumber = intercalate "-" . map digitToWord . digits
