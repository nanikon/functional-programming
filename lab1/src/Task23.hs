module Task23 (
    task23Spec,
    task23Map,
    task23Lazy,
) where

import Data.MemoTrie

intSqrt :: Int -> Int
intSqrt n = floor (sqrt (fromIntegral n :: Float))

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
        [2 .. intSqrt n]

isAbundant :: Int -> Bool
isAbundant = memo (\n -> divisionSum n > n)

isSumAb :: Int -> Bool
isSumAb n = any (\k -> isAbundant (n - k)) (filter isAbundant [1 .. n - 1])

task23Spec :: Int -> Int
task23Spec limit = sum $ filter (not . isSumAb) [1 .. limit]

-- map version

divisionsByMap :: Int -> [Int]
divisionsByMap n =
    1
        : map
            ( \x ->
                let t = n `div` x
                 in if n `mod` x == 0
                        then
                            if x == t
                                then x
                                else x + t
                        else 0
            )
            [2 .. intSqrt n]

divisionSumMap :: Int -> Int
divisionSumMap = sum . divisionsByMap

isAbundantMap :: Int -> Bool
isAbundantMap = memo (\n -> divisionSumMap n > n)

isSumAbMap :: Int -> Bool
isSumAbMap n = any (\k -> isAbundantMap (n - k)) [x | x <- [1 .. n - 1], isAbundantMap x]

task23Map :: Int -> Int
task23Map limit =
    sum $
        map
            ( \x ->
                if not (isSumAbMap x)
                    then x
                    else 0
            )
            [1 .. limit]

-- version with lazy computing

abundantNums :: [Int]
abundantNums = filter isAbundant [1 ..]

isSumTwoAb :: Int -> Bool
isSumTwoAb n = any (\k -> isAbundant (n - k)) (takeWhile (< n) abundantNums)

task23Lazy :: Int -> Int
task23Lazy limit = sum $ filter (not . isSumTwoAb) (takeWhile (<= limit) [1 ..])
