module PolygonArea where

import Control.Monad (replicateM)
import Text.Printf (printf)
import Util

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
  points <- replicateM n $ getLine >>= pure . extract2I
  let perimeter = calcArea points
  printf "%.1f" perimeter
