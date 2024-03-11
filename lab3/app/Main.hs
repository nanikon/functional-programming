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
    foldM_ handleWindow (reverse ((0.0, 0.0) : take firstOffset points)) (drop firstOffset points)
  where
    handleWindow acc point = do
        let window = changeWindow acc point
        putStr $ fmt $ "Current window: " +| formatPointList window |+ "\n"
        let xs = generateXs window (pointCount params)
        let linearResult = "Linear interpolation: " +| formatPointList (mapAndZip (linInterOnePoint window) xs) |+ "\n"
        let lagrangResult = "Lagrang interpolation: " +| formatPointList (mapAndZip (lagrangInterOnePoint window) xs) |+ "\n"
        when (useLinear params) $ putStr $ fmt linearResult
        when (useLinear params) $ putStr $ fmt lagrangResult
        unless (useLinear params || useLagrang params) $ putStr $ fmt $ linearResult <> lagrangResult
        return window
    mapAndZip f a = zip a (map f a)
