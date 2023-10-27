module Lib
    ( task8First
    ) where

myMultiply :: [Int] -> Int
myMultiply [] = 1
myMultiply (x:xs) = x * myMultiply xs

task8First :: [Int] -> Int
task8First = maxMult 0
  where
    maxMult n [] = n
    maxMult n (x:xs) = 
        let currentMult = myMultiply (take 13 (x:xs))
        in if currentMult > n
            then maxMult currentMult xs
            else maxMult n xs