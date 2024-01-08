module Lib
    ( someFunc
    ) where

import Data.Hashable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Elem a b = Elem a b

newtype Bucket a b = Bucket [Elem a b]
data SepChainHashMap a b = SepChainHashMap Int [Bucket a b]

addElem :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b 

--filter
--map
--foldl
--foldr
--monoid