module Main where

import Lib

swapHt :: [a] -> [a]
swapHt xs = tail : body : head where
  tail = (head . reverse) xs
  body = (tail . reverse . tail) xs
  head = head xs

main :: IO ()
main = someFunc
