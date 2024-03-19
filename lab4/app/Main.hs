module Main (main) where

import Lib

main :: IO ()
main = putChar $ resultParse $ runParser (satisfy (== 'a')) "a"
