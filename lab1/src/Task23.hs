module Task23 (
    task23Spec,
) where

import Data.MemoTrie

-- fold and spec function version

divisionSum :: Int -> Int
divisionSum n =
    foldr
        ( \x acc ->
            let t = n `div` x
             in if n `mod` x == 0
                    then
                        if x == t
                            then acc + x
                            else acc + x + t
                    else acc
        )
        1
        [2 .. floor (sqrt (fromIntegral n :: Float))]

isAbundant :: Int -> Bool
isAbundant = memo (\n -> divisionSum n > n)

isSumAb :: Int -> Bool
isSumAb n = any (\k -> isAbundant (n - k)) (filter isAbundant [1 .. n - 1])

task23Spec :: Int -> Int
task23Spec limit = sum $ filter (not . isSumAb) [1 .. limit]
