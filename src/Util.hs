module Util where

-- Utility functions for working with HackerRank.  Although they will
-- be copied per each solution.


import qualified Data.Text as T

split :: Char -> String -> [String]
split sep str = map T.unpack nonEmpty
  where
    splitted = T.split (==sep) (T.pack str)
    nonEmpty = filter (\x -> T.length x > 0) splitted

split' :: String -> [String]
split' = split ' '

extract2I :: String -> (Int, Int)
extract2I row = (x, y)
  where
    [x, y] = map read (split' row)



