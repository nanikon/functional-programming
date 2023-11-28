module Task8 (
    task8Recursion,
    task8_TailRec,
    task8Special,
    task8Map,
    task8Iterate,
) where

-- Non-tail recursion

task8Recursion :: [Int] -> Int
task8Recursion xs
    | length xs < 13 = error "List must have 13 elements"
    | length xs == 13 = product xs
    | otherwise = max (product . take 13 $ xs) (task8Recursion . tail $ xs)

-- Tail recursion

task8_TailRec :: [Int] -> Int
task8_TailRec = maxMult 0
  where
    maxMult n xs
        | length xs < 13 = error "List must have 13 elements"
        | length xs == 13 = max n $ product xs
        | otherwise = maxMult (max (product . take 13 $ xs) n) (tail xs)

-- module version with speshial functions

groupByNFold :: Int -> [Int] -> [[Int]]
groupByNFold n xs =
    foldr
        ( \el acc ->
            foldr
                ( \curList newList ->
                    if length curList < n then (el : curList) : newList else curList : newList
                )
                [[el]]
                acc
        )
        [[head xs]]
        (tail xs)

filterShortElem :: Int -> [[Int]] -> [[Int]]
filterShortElem n = filter $ \x -> length x >= n

filterWithout0 :: [[Int]] -> [[Int]]
filterWithout0 = filter $ notElem 0

multiplyInnerList :: [[Int]] -> [Int]
multiplyInnerList = foldl (\acc el -> product el : acc) []

task8Special :: [Int] -> Int
task8Special = maximum . multiplyInnerList . filterWithout0 . filterShortElem 13 . groupByNFold 13

-- version with map

groupByNMap :: Int -> [Int] -> [[Int]]
groupByNMap n list
    | n > length list = error "List must have more or equal n elements"
    | otherwise = map (\x -> take n . drop x $ list) [0 .. length list - n]

task8Map :: [Int] -> Int
task8Map = maximum . map product . groupByNMap 13

-- version with lazy compution

task8Iterate :: [Int] -> Int
task8Iterate list = maximum (take (length list - 13) [product . take 13 . drop x $ list | x <- [0 ..]])
