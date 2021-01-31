{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Map (Map, listToFM) where

import qualified Data.Map as M
import Data.List (intercalate)
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP ((+++), ReadP)
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), spaces)

newtype Map a k = Map (M.Map a k) deriving (Eq, Ord)

instance (Show a, Show k) => Show (Map a k) where
    show (Map m) = "listToFM [" ++ intercalate "," (map show $ M.toList m) ++ "]"

instance (Readable a, Readable k, Ord k) => Read (Map k a) where
    readsPrec n = P.readP_to_S readP

instance (Readable a, Readable k, Ord k) => Readable (Map k a) where
    readP =  Map . M.fromList <$> (spaces >> P.string "listToFM" >> spaces *> parseList)
        where parseList = readP :: ReadP [(k,a)]

instance (DAO (M.Map a k)) (Map a k) where
    toValue (Map m) = m

listToFM :: (Ord a) => [(a,b)] -> Map a b
listToFM = Map . M.fromList