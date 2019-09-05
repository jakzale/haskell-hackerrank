module PrefixCompression where

import Text.Printf (printf)

samePrefix :: Eq a => [a] -> [a] -> [a]
samePrefix x y = fst $ unzip $ prefix
  where
    prefix = takeWhile (uncurry (==)) zipped
    zipped = zip x y

showPrefix :: String -> String
showPrefix prefix = printf "%i %s" l prefix
  where
    l = length prefix

main :: IO ()
main = do
  x <- getLine
  y <- getLine
  let prefix = samePrefix x y
  let l = length prefix
  putStrLn $ showPrefix prefix
  let x' = drop l x
  let y' = drop l y
  putStrLn $ showPrefix x'
  putStrLn $ showPrefix y'
