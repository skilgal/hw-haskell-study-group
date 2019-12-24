module Module where

x :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show x)

data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
               then Blah
               else x

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePopus p p' = p > p'
