module PascalTriangle where

import Control.Monad
import Data.List


pascalTriangle' :: [Integer] -> [Integer]
pascalTriangle' x = fmap (uncurry (+)) (zip (0 : x) (reverse padded))
  -- trick, rows are symmetrical
  where padded = 0 : x

pascalTriangleRec x = triangle : pascalTriangleRec triangle
  where triangle = pascalTriangle' x

pascalTriangles = [1] : pascalTriangleRec [1]

pascalTriangle :: Int -> [[Integer]]
pascalTriangle n = take n pascalTriangles

main :: IO ()
main = do
  k <- readLn :: IO Int
  let triangle = pascalTriangle k
  forM_ triangle $ \row ->
    putStrLn $ intercalate " " $ fmap show row
