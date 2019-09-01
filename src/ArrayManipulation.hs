{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module ArrayManipulation where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad
import Control.Monad.ST
import Data.List
import Debug.Trace
import System.Environment
import System.IO

-- Complete the arrayManipulation function below.
arrayManipulation :: Int -> [(Int, Int, Int)] -> Int
arrayManipulation n queries =
  maximum values
  where
    calcChanges [] = []
    calcChanges ((a, b, k):rest) = (a, k):(b + 1, -k):calcChanges rest
    changes = filter ((<=n) . fst) (calcChanges queries)
    orderedChanges = sortOn fst changes
    groupedChanges = groupBy (\ a b -> (fst a) == (fst b)) orderedChanges
    summedGroupedChanges = map (sum . (map snd)) groupedChanges
    values = scanl' (+) 0 summedGroupedChanges

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray m
    let queries = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) queriesTemp
    let tuples = Data.List.map (\[x, y, z] -> (x, y, z)) queries
    let result = arrayManipulation n tuples

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
