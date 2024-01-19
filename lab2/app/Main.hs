module Main (main) where

import Lib

main :: IO ()
main = print (createHashMap 0.8 [('a', '1'), ('b', '2'), ('c', '3')])
