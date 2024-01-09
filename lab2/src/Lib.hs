module Lib (
    someFunc,
    createHashMap,
    deleteElem,
    getElem,
    getSize,
    getCurrentFilled,
    filledHashMap,
) where

import Data.Hashable
import qualified Data.List as DL
import Data.Ord ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Elem a b = (a, b)

type Bucket a b = [Elem a b]
data SepChainHashMap a b = SepChainHashMap
    { filledHashMap :: Double
    , dataHashMap :: [Bucket a b]
    }

createHashMap :: (Hashable a, Ord a) => Double -> [(a, b)] -> SepChainHashMap a b
-- addElem :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
-- filterHashMap :: (Hashable a) => (a -> b -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
-- mapHasMap :: (Hashable a, Hashable c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
-- foldlHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b
-- foldrHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b

getSize :: SepChainHashMap a b -> Int -- element count
getSize hM = sum (map length (dataHashMap hM))

getCurrentFilled :: (Eq a, Eq b) => SepChainHashMap a b -> Double
getCurrentFilled hM = fromIntegral (length (filter (/= []) (dataHashMap hM))) / fromIntegral (length (dataHashMap hM))

-- instance Semigroup (SepChainHashMap a b) where
--    (<>) = mergeTwoMap

-- instance Monoid (SepChainHashMap a b) where
--    mempty = SepChainHashMap 0 []

-- mergeTwoMap :: (Hashable a) => SepChainHashMap a b -> SepChainHashMap a b -> SepChainHashMap a b

-- utils

shortHash :: (Hashable a) => a -> Int -> Int
shortHash key len = hash key `mod` len

shortHashElem :: (Hashable a) => Int -> (a, b) -> Int
shortHashElem len e = shortHash (fst e) len

shortHashElemFromHeadBucket :: (Hashable a) => [Bucket a b] -> Int -> Int
shortHashElemFromHeadBucket data_ len = shortHashElem len (head (head data_))

compareRes :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareRes fun x y = compare (fun x) (fun y)

equalRes :: (Ord b) => (a -> b) -> a -> a -> Bool
equalRes fun x y = compareRes fun x y == EQ

-- createMap
getLenForHalfFilled :: Double -> Int -> Int
getLenForHalfFilled filled size = ceiling $ fromIntegral (2 * size) / filled

createHashMap filled pairs
    | (filled <= 0) || (filled >= 1) = error "Filled must be in (0; 1)"
    | otherwise = SepChainHashMap filled (makeHashMapData (getLenForHalfFilled filled (length pairs)) (deleteDublicates pairs))
  where
    makeHashMapData len data_ = fillProbels [] 0 len (groupAndSortByHash len data_)
    fillProbels result curMod len input
        | curMod == len = reverse result
        | null input = reverse result ++ replicate (len - curMod) []
        | curMod == shortHashElemFromHeadBucket input len = fillProbels (head input : result) (curMod + 1) len (tail input)
        | otherwise = fillProbels ([] : result) (curMod + 1) len input
    deleteDublicates = DL.nubBy (equalRes fst)
    groupAndSortByHash len xs = DL.sortBy (compareRes (fst . head)) (DL.groupBy (equalRes (shortHashElem len)) xs)

-- getElem
getNeededBucket :: (Hashable a) => SepChainHashMap a b -> a -> [(Int, Bucket a b)]
getNeededBucket hM key = filter (\b -> fst b == shortHash key (length (dataHashMap hM))) (zip [0 ..] (dataHashMap hM))

getNeededElem :: (Hashable a) => [Elem a b] -> a -> [(Int, Elem a b)]
getNeededElem b key = filter (\e -> fst (snd e) == key) (zip [0 ..] b)

getElem hM key =
    case getNeededBucket hM key of
        [] -> Nothing
        (_, b) : _ -> case getNeededElem b key of
            [] -> Nothing
            (_, (_, v)) : _ -> Just v

-- deleteElem

deleteElemFromBucket :: (Hashable a) => Bucket a b -> a -> Bucket a b
deleteElemFromBucket b key = case getNeededElem b key of
    [] -> b
    [(number, _)] -> take number b ++ getListAfterElem b number
    _ -> error "Not exclusive key"

getListAfterElem :: [a] -> Int -> [a]
getListAfterElem list n = take (length list - n - 1) . drop (n + 1) $ list

deleteElem hM key =
    case getNeededBucket hM key of
        [] -> hM
        (number, b) : _ -> SepChainHashMap (filledHashMap hM) (take number (dataHashMap hM) ++ deleteElemFromBucket b key : getListAfterElem (dataHashMap hM) number)
