module Lib (
    SepChainHashMap,
    createHashMap,
    deleteElem,
    getElem,
    addElem,
    filterHashMap,
    mapHashMap,
    foldlHashMap,
    foldrHashMap,
    getSizeMap,
    getCurrentFilled,
    filledHashMap,
) where

import Data.Hashable
import qualified Data.List as DL
import Data.Ord ()

type Elem a b = (a, b)

type Bucket a b = [Elem a b]
data SepChainHashMap a b = SepChainHashMap
    { filledHashMap :: Double
    , dataHashMap :: [Bucket a b]
    }

instance (Hashable a, Eq a, Eq b) => Eq (SepChainHashMap a b) where
    (==) x y = equalRes (length . concatData) x y && all (\a -> a `elem` concatData y) (concatData x)

instance (Show a, Show b) => Show (SepChainHashMap a b) where
    show x = "{ " ++ show (filledHashMap x) ++ " - " ++ show (dataHashMap x) ++ "}"

instance (Hashable a, Eq a) => Semigroup (SepChainHashMap a b) where
    (<>) x y = createHashMap (max (filledHashMap x) (filledHashMap y)) (concatData x ++ concatAndFilterData x y)
      where
        concatAndFilterData a = filter (\e -> fst e `notElem` map fst (concatData a)) . concatData

instance (Hashable a, Eq a) => Monoid (SepChainHashMap a b) where
    mempty = SepChainHashMap 0 []

-- utils

getSizeMap :: SepChainHashMap a b -> Int
getSizeMap hM = sum (map length (dataHashMap hM))

getCurrentFilled :: (Eq a, Eq b) => SepChainHashMap a b -> Double
getCurrentFilled hM = fromIntegral (length (filter (/= []) (dataHashMap hM))) / fromIntegral (length (dataHashMap hM))

concatData :: SepChainHashMap a b -> [Elem a b]
concatData = concat . dataHashMap

shortHash :: (Hashable a) => a -> Int -> Int
shortHash key len = abs (hash key) `mod` len

shortHashElem :: (Hashable a) => Int -> (a, b) -> Int
shortHashElem len e = shortHash (fst e) len

shortHashElemFromHeadBucket :: (Hashable a) => [Bucket a b] -> Int -> Int
shortHashElemFromHeadBucket data_ len = shortHashElem len (head (head data_))

compareRes :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareRes fun x y = compare (fun x) (fun y)

equalRes :: (Eq b) => (a -> b) -> a -> a -> Bool
equalRes fun x y = fun x == fun y

getListAfterElem :: [a] -> Int -> [a]
getListAfterElem list n = take (length list - n - 1) . drop (n + 1) $ list

-- createMap
getLenForHalfFilled :: Double -> Int -> Int
getLenForHalfFilled filled size = ceiling $ fromIntegral (2 * max size 1) / filled

createHashMap :: (Hashable a) => Double -> [(a, b)] -> SepChainHashMap a b
createHashMap filled pairs
    | (filled <= 0) || (filled >= 1) = error "Filled must be in (0; 1)"
    | otherwise = SepChainHashMap filled (makeHashMapData (getLenForHalfFilled filled (length (deleteDublicates pairs))) (deleteDublicates pairs))
  where
    makeHashMapData len data_ = fillProbels [] 0 len (groupAndSortByHash len data_)
    fillProbels result curMod len input
        | curMod == len = reverse result
        | null input = reverse result ++ replicate (len - curMod) []
        | curMod == shortHashElemFromHeadBucket input len = fillProbels (head input : result) (curMod + 1) len (tail input)
        | otherwise = fillProbels ([] : result) (curMod + 1) len input
    deleteDublicates = DL.nubBy (equalRes fst)
    groupAndSortByHash len xs = DL.groupBy (equalRes (shortHashElem len)) (DL.sortBy (compareRes (shortHashElem len)) xs)

-- getElem

getNeededBucket :: (Hashable a) => SepChainHashMap a b -> a -> (Int, Bucket a b)
getNeededBucket hM key = head $ filter (\b -> fst b == shortHash key (length (dataHashMap hM))) (zip [0 ..] (dataHashMap hM))

getNeededElem :: (Hashable a) => [Elem a b] -> a -> [(Int, Elem a b)]
getNeededElem b key = filter (\e -> fst (snd e) == key) (zip [0 ..] b)

getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
getElem hM key =
    case getNeededElem (snd (getNeededBucket hM key)) key of
        [] -> Nothing
        (_, (_, v)) : _ -> Just v

-- addElem

addElemWithoutCheckFill :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
addElemWithoutCheckFill hM key value =
    let (number, b) = getNeededBucket hM key
        d = dataHashMap hM
        newB = case b of
            [] -> [(key, value)]
            _ -> case getNeededElem b key of
                [] -> (key, value) : b
                (numberElem, (_, _)) : _ -> take numberElem b ++ (key, value) : getListAfterElem b numberElem
     in SepChainHashMap (filledHashMap hM) (take number d ++ newB : getListAfterElem d number)

checkFillAndExpand :: (Hashable a, Eq b) => SepChainHashMap a b -> SepChainHashMap a b
checkFillAndExpand hM
    | getCurrentFilled hM >= filledHashMap hM = createHashMap (filledHashMap hM) (concat . dataHashMap $ hM)
    | otherwise = hM

addElem :: (Hashable a, Eq b) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
addElem hM key value = checkFillAndExpand $ addElemWithoutCheckFill hM key value

-- deleteElem

deleteElemFromBucket :: (Hashable a) => Bucket a b -> a -> Bucket a b
deleteElemFromBucket b key = case getNeededElem b key of
    [] -> b
    (number, (_, _)) : _ -> take number b ++ getListAfterElem b number

deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
deleteElem hM key =
    let (number, b) = getNeededBucket hM key
     in SepChainHashMap (filledHashMap hM) (take number (dataHashMap hM) ++ deleteElemFromBucket b key : getListAfterElem (dataHashMap hM) number)

-- filter

filterHashMap :: ((a, b) -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
filterHashMap cond hM = SepChainHashMap (filledHashMap hM) (map (filter cond) (dataHashMap hM))

-- map

mapHashMap :: (Hashable c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
mapHashMap func hM = createHashMap (filledHashMap hM) (concatMap (map func) . dataHashMap $ hM)

-- fold

foldlHashMap :: (c -> Elem a b -> c) -> c -> SepChainHashMap a b -> c
foldlHashMap func startAcc = foldl (foldl func) startAcc . dataHashMap

foldrHashMap :: (Elem a b -> c -> c) -> c -> SepChainHashMap a b -> c
foldrHashMap func startAcc = foldr (flip (foldr func)) startAcc . dataHashMap
