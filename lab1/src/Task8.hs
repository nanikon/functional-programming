module Task8 (
    task8Recursion,
    task8_TailRec,
    task8Special,
    task8Map,
    task8Iterate,
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

groupByNFold :: Int -> [Int] -> [[Int]]
groupByNFold n =
    foldr
        ( \el acc ->
            case acc of
                (_ : _) ->
                    foldr
                        ( \curList newList ->
                            if length curList < n then (el : curList) : newList else curList : newList
                        )
                        [[el]]
                        acc
                _ -> [[el]]
        )
        []

filterShortElem :: Int -> [[Int]] -> [[Int]]
filterShortElem n = filter (\x -> length x >= n)

filterWithout0 :: [[Int]] -> [[Int]]
filterWithout0 = filter (notElem 0)

multiplyInnerList :: [[Int]] -> [Int]
multiplyInnerList = foldl (\acc el -> product el : acc) []

task8Special :: [Int] -> Int
task8Special = maximum . multiplyInnerList . filterWithout0 . filterShortElem 13 . groupByNFold 13

-- version with map

groupByNMap :: Int -> [Int] -> [[Int]]
groupByNMap n list =
    if n > length list
        then error "List must have more or equal n elements"
        else map (\x -> take n (drop x list)) [0 .. length list - n]

task8Map :: [Int] -> Int
task8Map = maximum . map product . groupByNMap 13

-- version with lazy compution

task8Iterate :: [Int] -> Int
task8Iterate list = maximum (take (length list - 13) [product (take 13 (drop x list)) | x <- [0 ..]])
