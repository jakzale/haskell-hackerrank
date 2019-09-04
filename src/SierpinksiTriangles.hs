module SierpinskiTriangles where

import Data.Set

type Point = (Int, Int)
type Triangle = (Point, Int)

mkTriangle x y z = ((x, y), z)

trianglePoints :: Point -> Int -> [Point]
trianglePoints top height = trianglePoints' top 0
  where
    trianglePoints' :: Point -> Int -> [Point]
    trianglePoints' (x, y) currHeight
      | currHeight < height = pointsRow (x, y) currHeight
                              ++ trianglePoints' (x, y + 1) (currHeight + 1)
      | otherwise = []
    
pointsRow :: Point -> Int -> [Point]
pointsRow (x, y) halfW = [(x1, y) | x1 <- [x - halfW .. x + halfW]]

splitTriangle :: Triangle -> [Triangle]
splitTriangle ((x, y), h) = [top, left, right]
  where
    h' = h `quot` 2
    y' = y + h'
    x1 = x - h'
    x2 = x + h'
    top = mkTriangle x y h'
    left = mkTriangle x1 y' h'
    right = mkTriangle x2 y' h'
    
render :: Set Point -> Point -> Char
render s p = if member p s then '1' else '_'

startingTriangle = [mkTriangle 31 0 32]

renderPoints :: Int -> Int -> [Point] -> [String]
renderPoints cols rows points =
  [[ renderPoint (x, y)  | x <- [ 0 .. cols - 1 ]] | y <- [ 0 .. rows - 1 ]]
  where
    pointSet = Data.Set.fromList points
    renderPoint = render pointSet

splitRec :: Int -> [Triangle] -> [Triangle]
splitRec n triangles
  | n > 0 = splitRec (n - 1) (triangles >>= splitTriangle)
  | otherwise = triangles

sierpinski :: Int -> String
sierpinski n = unlines renderedPoints
  where
    renderedPoints = renderPoints 63 32 points
    points = triangles >>= uncurry trianglePoints
    triangles = splitRec n startingTriangle

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ sierpinski n
