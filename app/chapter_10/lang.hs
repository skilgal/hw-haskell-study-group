module Language where

capitalizeWord :: String -> String
capitalizeWord (w : ws) =

capitalizeParagraph :: String -> String
capitalizeParagraph = (intercalate ".") . (map capitalizeWord) . (splitOn ".")


testCap1 :: IO ()
testCap1 =
  if capitalizeWord "Chortle" == "Chortle"
  then putStrLn "Okay"
  else error "Impl is incorrect"

testCap2 :: IO ()
testCap2 =
  if capitalizeWord "chortle" == "Chortle"
  then putStrLn "Okay"
  else error "Impl is incorrect"

main :: IO ()
main = do
  testCap1
  testCap2
