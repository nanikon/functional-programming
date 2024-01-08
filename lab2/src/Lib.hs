module Lib (
    someFunc,
) where

import Data.Hashable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Elem a b = (a, b)

newtype Bucket a b = Bucket [Elem a b]
data SepChainHashMap a b = SepChainHashMap
    { sizeHashMap :: Int
    , dataHashMap :: [Bucket a b]
    }

addElem :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
filterHashMap :: (Hashable a) => (a -> b -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
mapHasMap :: (Hashable a, Hashable c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
foldlHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b
foldrHashMap :: (Hashable a) => (Elem a b -> Elem a b -> Elem a b) -> SepChainHashMap a b -> Elem a b

instance Semigroup (SepChainHashMap a b) where
    (<>) = mergeTwoMap

instance Monoid (SepChainHashMap a b) where
    mempty = SepChainHashMap 0 []

mergeTwoMap :: (Hashable a) => SepChainHashMap a b -> SepChainHashMap a b -> SepChainHashMap a b
getNeededBucket :: (Hashable a) => SepChainHashMap a b -> a -> [(Int, Bucket a b)]
getNeededBucket hM key = filter (\b -> fst b == hash key `mod` length dataHashMap hm) (zip [0 ..] (dataHashMap hm))

getNeededElem :: (Hashable a) => Bucket a b -> a -> [Elem a b]
getNeededElem b key = filter (\e -> fst e == key) b

getElem hM key =
    case getNeededBucket hM key of
        [] -> Nothing
        [(number, b)] -> case getNeededElem b key of
            [] -> Nothing
            (k, v) : _ -> Just v
