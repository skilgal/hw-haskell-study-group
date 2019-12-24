module Reverse where

swap :: String -> String
swap str = swapCount (length (words str) - 1) str

swapCount :: Int -> String -> String
swapCount 1 str = swapOnce str
swapCount count str = swapCount (count - 1) (swapOnce str)

swapOnce :: String -> String
swapOnce str = unwords (drop 1 splitted ++ take 1 splitted)
  where splitted = words str

        -- Actually homework

rvrs :: String -> String
rvrs str = unwords (rvrs2 $ words str)

rvrs2 :: [a] -> [a]
rvrs2 [] = []
rvrs2 (h : t) = rvrs2 t ++ [h]

main :: IO ()
main = print $ rvrs "Curry is awesome"
