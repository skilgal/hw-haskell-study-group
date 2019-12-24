module UnderstandingFolds where


main :: IO ()
main = do
  print $ foldr (*) 1 [1..5]
  -- print $ flip (*) 1 [1..5] -- ERROR: can't multiply diff types
  print $ foldl (flip (*)) 1 [1..5]
  print $ foldl (*) 1 [5]
  print $ foldr (++) "" ["", "", ""]
  print $ foldr max '0' "some text"
  print $ foldr (&&) True [False, True]
  print $ foldr (||) True [False, True]
  print $ foldl (++) "" (map show [1..5])
