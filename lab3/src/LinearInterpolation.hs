module LinearInterpolation (
    linInter
) where

import PointWindow

linInterOnePoint :: PointWindow -> Double -> Double
linInterOnePoint w x = let
    number = length . takeWhile (\e -> fst e > x) $ w
    point1 = w !! number
    point0 = w !! (number + 1)
    y1 = snd point1
    y0 = snd point0
    x1 = fst point1
    x0 = fst point0
    in y0 + (y1 - y0) / (x1 - x0) * (x - x0)

linInter :: PointWindow -> [Double] -> [Point]
linInter w xs = zip xs $ map (linInterOnePoint w) xs
