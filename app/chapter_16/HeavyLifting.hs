{-# LANGUAGE FlexibleContexts #-}
module HeavyLifting where

-- (+1) $ (read "[1]" :: [Int])
a :: [Int]
a = fmap (+1) (read "[1]" :: [Int])

-- map (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- (*2) (\x -> x - 2)
c :: Int -> Int
c = fmap (*2) (\x -> x - 2)

-- ((return '1' ++) . show)
-- (\x -> [x, 1..3])
d :: Int -> String
d = fmap ((return '1' ++) . show)
    (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

main :: IO ()
main = do
  putStrLn $ "a = " ++  (show a)
  putStrLn $ "b = " ++  (show b)
  putStrLn $ "c = " ++  (show . c) 1
  putStrLn $ "d = " ++  ((show . d) 0)
  putStr "e = "
  show <$> e >>= putStrLn
  return ()
