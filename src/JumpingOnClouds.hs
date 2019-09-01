{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module JumpingOnClouds where

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

-- Complete the jumpingOnClouds function below.
jumpingOnClouds :: [Int] -> Int
jumpingOnClouds c = joc 0 (Data.List.drop 1 c)

joc :: Int -> [Int] -> Int
joc acc [] =  acc
joc acc [0] = (acc + 1)
joc acc (_:0:rest) = joc (acc + 1) rest
joc acc (0:rest) = joc (acc + 1) rest
joc _ _ = undefined

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    cTemp <- getLine

    let c = Data.List.map (read :: String -> Int) . words $ cTemp

    let result = jumpingOnClouds c

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
