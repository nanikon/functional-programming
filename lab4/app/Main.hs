module Main (main) where

import Lib

main :: IO ()
main = putChar $ resultParse $ parse (satisfy (== 'a')) "ab"
