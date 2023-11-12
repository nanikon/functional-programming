module Task8 (
    task8_TailRec,
) where

myMultiplyTailRec :: [Int] -> Int
myMultiplyTailRec = myMultiply_inner 1
  where
    myMultiply_inner n [] = n
    myMultiply_inner n (x : xs) = myMultiply_inner (n * x) xs

task8_TailRec :: [Int] -> Int
task8_TailRec = maxMult 0
  where
    maxMult n [] = n
    maxMult n (x : xs) =
        let currentMult = myMultiplyTailRec (take 13 (x : xs))
         in if currentMult > n
                then maxMult currentMult xs
                else maxMult n xs
