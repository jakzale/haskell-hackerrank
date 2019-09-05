module StringReductions where

import qualified Data.Set as Set
import Data.Set(Set)

stringReductions :: String -> String
stringReductions str = reverse revReduced
  where
    revReduced = snd redState
    redState = foldr reduceString (Set.empty, []) (reverse str)

-- modelling as fold
reduceString :: Char ->  (Set Char, [Char]) -> (Set Char, [Char])
reduceString c (seen, acc) 
  | c `Set.member` seen = (seen, acc)
  | otherwise = ((c `Set.insert` seen), c:acc)


main :: IO ()
main = do
  s <- getLine
  putStrLn $ stringReductions s
