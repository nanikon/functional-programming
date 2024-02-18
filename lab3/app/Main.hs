module Main (main) where

import Lib
import Data.Functor

main :: IO ()
main = getContents >>= putStr . take 4 .split
