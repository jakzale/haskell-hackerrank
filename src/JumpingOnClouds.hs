module JumpingOnClouds where

import Data.List
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

parseInt :: String -> Int
parseInt = read

parseInts :: String -> [Int]
parseInts = fmap parseInt . words

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
    c <- parseInts <$> getLine

    let result = jumpingOnClouds c

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
