module Fib where

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
