module Autotool.Util.Hash (hnub) where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.List (foldl')
import Data.Bits (Bits(xor))

hnub :: (Show a) => [a] -> [a]
hnub xs = go IS.empty xs [] where
    go :: (Show a) => IntSet -> [a] -> [a] -> [a]
    go _ [] ys = ys
    go hs (x:xs) ys =
        let h = hash $ show x
        in if IS.member h hs then go hs xs ys else go (IS.insert h hs) xs (x:ys)
    hash :: String -> Int
    hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381