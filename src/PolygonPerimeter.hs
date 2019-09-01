module PolygonPerimeter where

import Util
import Control.Monad (replicateM, replicateM_)
import Text.Printf (printf)

calcPerimeter :: [(Int, Int)] -> Double
calcPerimeter points = sum distances
  where
    sides' = sides points
    distances = map distance sides'

sides :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
sides points = zip points (drop 1 (cycle points))

distance :: ((Int, Int), (Int, Int)) -> Double
distance ((x1, y1), (x2, y2)) =
  sqrt ((x' ** 2) + (y' ** 2))
  where
    [x', y'] = map fromIntegral [x, y]
    x = x2 - x1
    y = y2 - y1

points :: [(Int, Int)]
points =
  [ (0, 0)
  , (0, 1)
  , (1, 1)
  , (1, 0)
  ]

main :: IO ()
main = do
  n <- readLn :: IO Int
  points <- replicateM n $ getLine >>= pure . extract2I
  let perimeter = calcPerimeter points
  printf "%.1f" perimeter
