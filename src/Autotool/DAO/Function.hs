{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Function ( Function(..), maplet ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Text.ParserCombinators.ReadP ( (+++), ReadP, readP_to_S)
import Autotool.Readable ( Readable(readP) )
import Autotool.DAO (toValue)
import Autotool.DAO.Set ( Set )

data Function a = Fn0 (Set a) | Fn1 (Set (a,a)) | Fn2 (Set (a,a,a)) deriving (Eq,Ord)

instance (Show a) => Show (Function a) where
    show (Fn0 s) = show s
    show (Fn1 s) = show s
    show (Fn2 s) = show s

instance (Readable a, Ord a) => Readable (Function a) where
    readP = parseFn0 +++ parseFn1 +++ parseFn2
        where
            parseFn0 = Fn0 <$> (readP :: ReadP (Set a))
            parseFn1 = Fn1 <$> (readP :: ReadP (Set (a,a)))
            parseFn2 = Fn2 <$> (readP :: ReadP (Set (a,a,a)))

instance (Readable a, Ord a) => Read (Function a) where
    readsPrec n = readP_to_S readP

maplet :: (Ord a) => Function a -> ([a] -> a)
maplet (Fn0 set) = let s = toValue set; [v] = S.toList s in const v
maplet (Fn1 set) = let s = toValue set; m = M.fromList $ S.toList s in (M.!) m . head
maplet (Fn2 set) = let s = toValue set; m = M.fromList $ map (\(a,b,c) -> ((a,b),c)) $ S.toList s in (M.!) m . (\[a,b] -> (a,b))