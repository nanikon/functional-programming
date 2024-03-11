module PointWindow (
    Point,
    PointWindow,
    changeWindow,
    generateXs,
    parsePoint,
) where

import Data.List.Split

type Point = (Double, Double)

-- in tail order
type PointWindow = [Point]

changeWindow :: PointWindow -> Point -> PointWindow
changeWindow w p = p : take (length w - 1) w

getLastPoint :: PointWindow -> Point
getLastPoint w = w !! (length w - 1)

generateXs :: PointWindow -> Int -> [Double]
generateXs [] _ = []
generateXs w@(p : _) n =
    let
        minX = fst . getLastPoint $ w
        period = (fst p - minX) / fromIntegral (n + 1)
     in
        scanl (+) (minX + period) (replicate (n - 1) period)

parsePoint :: String -> Point -- rewrite on read?
parsePoint s =
    let numbers = splitOn ";" s
     in (read (head numbers), read (numbers !! 1))
