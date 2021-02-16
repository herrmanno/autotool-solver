{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.MultiSet (MultiSet, mkMSet, MultiSetOp) where

import Prelude hiding ((+), (-))
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), comma, spaces, openBracelet, closeBracelet, number, spacedString)
import Autotool.Data.LazyTree (Op)
import qualified Autotool.Data.MultiSet as MS (MultiSet)
import qualified Autotool.Data.MultiSetOp as MS ((+), (&), (-))


newtype MultiSet a = MultiSet (M.Map a Int) deriving (Eq, Ord)

mkMSet :: (Ord a) => [(a,Int)] -> MultiSet a
mkMSet = MultiSet . M.fromList

instance (Show a) => Show (MultiSet a) where
    show (MultiSet m) = "{" ++ intercalate "," (map (\(k,v) -> show k ++ ":" ++ show v) $ M.toList m) ++ "}"

instance (Readable a, Ord a) => Read (MultiSet a) where
    readsPrec n = P.readP_to_S readP

instance (Readable a, Ord a) => Readable (MultiSet a) where
    readP = spaces *> readMultiSet <* spaces
        where
            readMultiSet = MultiSet . M.fromList <$> P.between openBracelet closeBracelet readList
            readList = spaces *> P.sepBy readEntry comma <* spaces
            readEntry = (,) <$> readEl <*> (spacedString ":" *> number <* spaces)
            readEl = spaces *> (readP :: P.ReadP a) <* spaces

instance (Ord b, (DAO b) a ) => (DAO (M.Map b Int)) (MultiSet a) where
    toValue (MultiSet m) = M.mapKeys toValue m


{- MULTISET OPERATORS -}

data MultiSetOp = OpDisjointUnion | OpIntersection | OpDiff

instance Show MultiSetOp where
    show OpDisjointUnion = "+"
    show OpIntersection = "&"
    show OpDiff = "-"

instance Read MultiSetOp where
    readsPrec _ = P.readP_to_S readP

instance Readable MultiSetOp where
    readP = readUnion <|> readIntersection <|> readDiff
        where
            readUnion = (spaces >> P.char '+' >> spaces) $> OpDisjointUnion
            readIntersection = (spaces >> P.char '&' >> spaces) $> OpIntersection
            readDiff = (spaces >> P.char '-' >> spaces) $> OpDiff

instance (Ord a) => (DAO (Op c (MS.MultiSet a))) MultiSetOp where
    toValue OpDisjointUnion = (MS.+)
    toValue OpIntersection = (MS.&)
    toValue OpDiff = (MS.-)