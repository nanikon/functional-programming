module Task8 (
    task8Recursion,
    task8_TailRec,
    task8Special,
) where

-- Non-tail recursion

task8Recursion :: [Int] -> Int
task8Recursion [] = error "List must have 13 elements"
task8Recursion [_] = error "List must have 13 elements"
task8Recursion [_, _] = error "List must have 13 elements"
task8Recursion [_, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [_, _, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
task8Recursion [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] = product [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]
task8Recursion (x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs) = max (product [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]) (task8Recursion (x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs))

-- Tail recursion

task8_TailRec :: [Int] -> Int
task8_TailRec = maxMult 0
  where
    maxMult _ [] = error "List must have 13 elements"
    maxMult _ [_] = error "List must have 13 elements"
    maxMult _ [_, _] = error "List must have 13 elements"
    maxMult _ [_, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult _ [_, _, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
    maxMult n [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] = max n $ product [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]
    maxMult n (x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs) =
        maxMult (max (product [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]) n) (x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs)

-- module version with speshial functions

groupBy13 :: [Int] -> [[Int]]
groupBy13 [] = error "List must have 13 elements"
groupBy13 [_] = error "List must have 13 elements"
groupBy13 [_, _] = error "List must have 13 elements"
groupBy13 [_, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [_, _, _, _, _, _, _, _, _, _, _, _] = error "List must have 13 elements"
groupBy13 [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] = [[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]]
groupBy13 (x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs) = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] : groupBy13 (x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : x13 : xs)

multiplyInnerList :: [[Int]] -> [Int]
multiplyInnerList = foldr (\v a -> product v : a) []

task8Special :: [Int] -> Int
task8Special = maximum . multiplyInnerList . groupBy13
