{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

-- Solution for Counting Valleys problem from HackerRank

module CountingValleys where

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

-- Complete the countingValleys function below.
countingValleys :: Int -> String -> Int
countingValleys n s = do
  length valleys
  where
    heightDiffs = fmap calcHeightDiff s
    heights = scanl' (+) 0 heightDiffs
    mountainsAndValleys = Data.List.filter (not . Data.List.null) (Data.List.Split.splitOn [0] heights)
    valleys = Data.List.filter ((<0) . (!!0)) mountainsAndValleys

calcHeightDiff :: Char -> Int
calcHeightDiff 'U' = 1
calcHeightDiff 'D' = -1
calcHeightDiff _ = undefined

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    s <- getLine

    let result = countingValleys n s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
