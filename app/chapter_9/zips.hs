module Zip where

import Prelude hiding (zip, zipWith)

zip :: [a] -> [b] -> [(a, b)]
zip (a : as) (b : bs) = (a, b) : zip as bs
zip _ _ = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

main :: String -> IO ()
main x = do
  putStrLn "Here we go"
  print $ show x
