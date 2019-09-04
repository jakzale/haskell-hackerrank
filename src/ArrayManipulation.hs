module ArrayManipulation where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Function
import Debug.Trace
import System.Environment
import System.IO

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

getNLines :: Int -> IO [String]
getNLines n = replicateM n getLine

-- Complete the arrayManipulation function below.
arrayManipulation :: Int -> [(Int, Int, Int)] -> Int
arrayManipulation n queries =
  maximum values
  where
    calcChanges [] = []
    calcChanges ((a, b, k):rest) = (a, k):(b + 1, -k):calcChanges rest
    changes = filter ((<=n) . fst) (calcChanges queries)
    orderedChanges = sortOn fst changes
    groupedChanges = groupBy ((==) `on` fst) orderedChanges
    summedGroupedChanges = map (sum . (map snd)) groupedChanges
    values = scanl' (+) 0 summedGroupedChanges


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    (n, m) <- parse2I <$> getLine
    tuples <- fmap parse3I <$> getNLines m

    let result = arrayManipulation n tuples

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
