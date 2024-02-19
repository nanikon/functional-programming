module LagrangInterpolation (
  lagrangInterOnePoint
) where

import PointWindow
import Data.StableMemo

-- make multiply by differenses second arg and first arg, without element in n index
makeMultiplyByDiff :: [Double] -> Double -> Int -> Double
makeMultiplyByDiff listX element number = makeMultiplyByDiff' listX element number 1 0
  where
    makeMultiplyByDiff' xs x n result cur_ptr
      | length xs == cur_ptr = result
      | n == cur_ptr = makeMultiplyByDiff' xs x n result (cur_ptr + 1)
      | otherwise = makeMultiplyByDiff' xs x n (result * (x - (xs !! cur_ptr))) (cur_ptr + 1)

makeDivider :: [Double] -> Int -> Double
makeDivider xs n = makeMultiplyByDiff xs (xs !! n) n

makeConstMult :: PointWindow -> Int -> Double
makeConstMult = memo2 (\w n -> snd (w !! n) / makeDivider (map fst w) n)

computeOnePolynom :: PointWindow -> Double -> Int -> Double
computeOnePolynom w x n = makeConstMult w n * makeMultiplyByDiff (map fst w) x n

lagrangInterOnePoint :: PointWindow -> Double -> Double
lagrangInterOnePoint w x = sum (map (computeOnePolynom w x) [0..length w - 1])
