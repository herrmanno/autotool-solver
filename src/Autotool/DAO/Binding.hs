{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Autotool.DAO.Binding (Binding, toPair, fromPair) where

import Text.ParserCombinators.ReadP (readP_to_S)
import Autotool.Readable (Readable(..), spacedString)
import Autotool.DAO (DAO(..))
import qualified Autotool.DAO.Identifier as DAO

data Binding a = Binding DAO.Identifier a deriving (Ord,Eq)

instance Functor Binding where
    fmap f (Binding i a) = Binding i $ f a

instance (Show a) => Show (Binding a) where
    show (Binding i a) = show i ++ " = " ++ show a

instance (Readable a) => Readable (Binding a) where
    readP = Binding <$> readP <* spacedString "=" <*> readP

instance (Readable a) => Read (Binding a) where
    readsPrec n = readP_to_S readP

instance ((DAO b) a) => (DAO (Char, b)) (Binding a) where
    toValue (Binding i a) = (toValue i, toValue a)

toPair :: Binding a -> (DAO.Identifier, a)
toPair (Binding i a)= (i, a)

fromPair :: Readable a => (DAO.Identifier, a) -> Binding a
fromPair = uncurry Binding