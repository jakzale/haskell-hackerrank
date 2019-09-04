module Util where

-- Utility functions for working with HackerRank.  They need to be
-- copied to each solution.  WARNING: These are quick and dirty, not
-- to used in prod code.

import Control.Monad (replicateM)

parseInt :: String -> Int
parseInt = read 

parseInts :: String -> [Int]
parseInts = map parseInt . words

parse2I :: String -> (Int, Int)
parse2I str = (a, b)
  where
    [a, b] = parseInts str

parse3I :: String -> (Int, Int, Int)
parse3I str = (a, b, c)
  where
    [a, b, c] = parseInts str

parse4I :: String -> (Int, Int, Int, Int)
parse4I str = (a, b, c, d)
  where
    [a, b, c, d] = parseInts str

getNLines :: Int -> IO [String]
getNLines n = replicateM n getLine
