module PolygonArea where

import Control.Monad (replicateM)
import Text.Printf (printf)

parseInt :: String -> Int
parseInt = read

parseInts :: String -> [Int]
parseInts = fmap parseInt . words

parse2I :: String -> (Int, Int)
parse2I str = (a, b)
  where
    [a, b] = parseInts str

calcArea :: [(Int, Int)] -> Double
calcArea points = sum triangleAreas
  where
    triangleAreas = map triangleArea triangles
    triangles = tasselate points

triangleArea :: ((Int, Int), (Int, Int), (Int, Int)) -> Double
triangleArea ((x1, y1), (x2, y2), (x3, y3)) =
  fromIntegral (x1y2 + x2y3 + x3y1 - x1y3 - x2y1 - x3y2) / 2.0
  where
    x1y2 = x1 * y2
    x2y3 = x2 * y3
    x3y1 = x3 * y1
    x1y3 = x1 * y3
    x2y1 = x2 * y1
    x3y2 = x3 * y2
    
tasselate :: [(Int, Int)] -> [((Int, Int), (Int, Int), (Int, Int))]
tasselate ((x1, y1):rest) = map mkTriangle (zip rest (drop 1 rest))
  where
    mkTriangle ((x2, y2), (x3, y3)) = ((x1, y1), (x2, y2), (x3, y3))



main :: IO ()
main = do
  n <- readLn :: IO Int
  points <- replicateM n $ parse2I <$> getLine
  let perimeter = calcArea points
  printf "%.1f" perimeter
