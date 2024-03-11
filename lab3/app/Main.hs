{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CmdParams
import Control.Monad
import Fmt
import LagrangInterpolation
import LinearInterpolation
import PointWindow

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
    input <- getContents
    let points = map parsePoint $ words input
    let firstOffset = windowSize params - 1
    foldM_
        ( \acc point -> do
            let window = changeWindow acc point
            putStr $ fmt $ "Current window: " +| foldl1 (\a b -> a <> ", " <> b) (map tupleF window) |+ "\n"
            let xs = generateXs window (pointCount params)
            let linearResult = "Linear interpolation: " +| foldl1 (<>) (zipWith (curry tupleF) xs (map (linInterOnePoint window) xs)) |+ "\n" -- rewrite on zip and fold
            let lagrangResult = "Lagrang interpolation: " +| foldl1 (<>) (zipWith (curry tupleF) xs (map (lagrangInterOnePoint window) xs)) |+ "\n"
            when (useLinear params) $ putStr $ fmt linearResult
            when (useLinear params) $ putStr $ fmt lagrangResult
            unless (useLinear params || useLagrang params) $ putStr $ fmt $ linearResult <> lagrangResult
            return window
        )
        (reverse ((0.0, 0.0) : take firstOffset points))
        (drop firstOffset points)
