module Main (main) where

import Lib

main :: IO ()
main = putChar $ resultParse $ runParser (choice (satisfy (== 'a')) (satisfy (== 'b'))) "b"
