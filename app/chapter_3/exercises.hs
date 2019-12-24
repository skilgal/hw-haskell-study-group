module Exercises where

a :: String -> String
a str = str ++ "!"

b :: String -> String
b str = take 1 (drop 4 str)

c :: String -> String
c = drop 9

f :: String -> Char
f x = x !! 3

fExt :: Int -> Char
fExt = (!!) "Curry is awesome"


main :: IO()
main = putStrLn $ a ""
