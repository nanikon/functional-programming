module Main (main) where

import Lib

main :: IO ()
main = putChar $ resultParse $ parse (bind (satisfy (== 'a')) (satisfy (== 'b'))) "ab"
