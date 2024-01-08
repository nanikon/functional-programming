module Lib (
    someFunc,
) where

import Data.Hashable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Elem a b = (a, b)

type Bucket a b = [Elem a b]
data SepChainHashMap a b = SepChainHashMap
    { filledHashMap :: Double
    , dataHashMap :: [Bucket a b]
    }

-- addElem :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
-- filterHashMap :: (Hashable a) => (a -> b -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
-- mapHasMap :: (Hashable a, Hashable c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
-- foldlHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b
-- foldrHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b

-- instance Semigroup (SepChainHashMap a b) where
--    (<>) = mergeTwoMap

-- instance Monoid (SepChainHashMap a b) where
--    mempty = SepChainHashMap 0 []

-- mergeTwoMap :: (Hashable a) => SepChainHashMap a b -> SepChainHashMap a b -> SepChainHashMap a b

-- getElem 
getNeededBucket :: (Hashable a) => SepChainHashMap a b -> a -> [(Int, Bucket a b)]
getNeededBucket hM key = filter (\b -> fst b == hash key `mod` length (dataHashMap hM)) (zip [0 ..] (dataHashMap hM))

getNeededElem :: (Hashable a) => [Elem a b] -> a -> [(Int, Elem a b)]
getNeededElem b key = filter (\e -> fst (snd e) == key) (zip [0..] b)

getElem hM key =
    case getNeededBucket hM key of
        [] -> Nothing
        (_, b):_ -> case getNeededElem b key of
            [] -> Nothing
            (_,(_, v)) : _ -> Just v

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
        (number, b):_ -> SepChainHashMap (filledHashMap hM) (take number (dataHashMap hM) ++ deleteElemFromBucket b key:getListAfterElem (dataHashMap hM) number)
