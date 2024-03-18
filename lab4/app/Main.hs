module Main (main) where

import Lib

main :: IO ()
main = putChar $ resultParse $ satisfy (== 'a') "ab"
