{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Set (Set, mkSet, mkBrSet) where

import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Data.List (intercalate)
import Text.ParserCombinators.ReadP ((+++))
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), comma, spaces, openBracket, closeBracket, openBracelet, closeBracelet)

data Set a = Set (S.Set a) | BrSet (S.Set a) deriving (Eq, Ord)

instance (Show a) => Show (Set a) where
    show (Set s) = "mkSet [" ++ intercalate "," (map show $ S.toList s) ++ "]"
    show (BrSet s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"

instance (Readable a, Ord a) => Read (Set a) where
    readsPrec n = P.readP_to_S readP

instance (Readable a, Ord a) => Readable (Set a) where
    readP = spaces *> (readSet +++ readBrSet) <* spaces
        where
            readSet = Set . S.fromList <$> ((P.string "mkSet" >> spaces) *> P.between openBracket closeBracket readList)
            readBrSet = BrSet . S.fromList <$> P.between openBracelet closeBracelet readList
            readList = P.sepBy readEl comma <* spaces
            readEl = (readP :: P.ReadP a) <* spaces

instance (DAO (S.Set a)) (Set a) where
    toValue (Set s) = s
    toValue (BrSet s) = s

instance (Ord a) => (DAO (Set a)) (S.Set a) where
    toValue = mkBrSet . S.toList

-- |Only for internal usage
mkSet :: (Ord a) => [a] -> Set a
mkSet = Set . S.fromList

-- |Only for internal usage
mkBrSet :: (Ord a) => [a] -> Set a
mkBrSet = BrSet . S.fromList