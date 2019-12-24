module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- Implement this
-- myLines :: String -> [String]
myLines = undefined

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

a = [ (x, y) |
  x <- mySqr,
  y <- myCube,
  x < 50, y < 50]
