{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module RepeatedString where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the repeatedString function below.
repeatedString :: String -> Integer -> Integer
repeatedString s n =
  q * aCount + (sum (Data.List.take (fromIntegral r) aOccur))
  where
    isA x = if x == 'a' then 1 else 0
    aOccur = fmap isA s
    aCount = sum aOccur
    l = length s
    (q, r) = n `quotRem` (fromIntegral l)
    

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    n <- readLn :: IO Integer

    let result = repeatedString s n

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
