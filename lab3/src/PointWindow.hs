module PointWindow (
    Point,
    PointWindow,
    changeWindow,
    generateXs,
) where 

type Point = (Double, Double)
-- in tail order
type PointWindow = [Point]

changeWindow :: PointWindow -> Point -> PointWindow 
changeWindow w p = p : take (length w - 1) w
-- changeWindow w p = tail w ++ [p]

getLastPoint :: PointWindow -> Point
getLastPoint w = w !! (length w - 1)

generateXs :: PointWindow -> Int -> [Double]
generateXs w@(p:ps) n = let
    minX = fst . getLastPoint $ w
    period = (fst p - minX) / fromIntegral n
    in scanl (+) (minX + period) (replicate (n - 1) period)