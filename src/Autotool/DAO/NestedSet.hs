{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.NestedSet (NestedSet, SetOp) where

import Prelude hiding ((+), (-), (&))
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP)
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), comma, spaces, openBracelet, closeBracelet)
import Autotool.Data.NestedSet (NSet, T, value, set, isValue, isSet, unwrapValue, unwrapSet, unwrap)
import Autotool.Data.SetOp ((+), (-), (&), pow)
import Autotool.Data.LazyTree (Op)

{- NESTED SET -}

newtype NestedSet a = NestedSet (S.Set (T a)) deriving (Eq, Ord)

instance (Show a) => Show (NestedSet a) where
    show (NestedSet s) = "{" ++ intercalate ", " (map toStr (S.toList s)) ++ "}"
        where
            toStr t
                | isValue t = show (unwrapValue t)
                | isSet t = "{" ++ intercalate ", " (map toStr (S.toList (unwrapSet t))) ++ "}"

instance (Readable a, Ord a) => Read (NestedSet a) where
    readsPrec n = P.readP_to_S readP

instance (Readable a, Ord a) => Readable (NestedSet a) where
    readP = NestedSet . unwrap <$> (spaces *> (parseEmpty <|> parseNonEmpty) <* spaces)
        where
            parseEmpty = spaces >> P.between openBracelet closeBracelet spaces $> (set S.empty :: T a)
            parseNonEmpty = spaces *> P.between openBracelet closeBracelet parseValues <* spaces
            parseValues = set . S.fromList <$> parseValue `P.sepBy1` comma
            parseValue = (readValue <|> parseEmpty <|> parseNonEmpty) <* spaces
            readValue = value <$> readP :: ReadP (T a)

instance (DAO (NSet a)) (NestedSet a) where
    toValue (NestedSet s) = s


{- (NESTED) SET OPERATIONS -}

data SetOp a = OpSum | OpUnion | OpDiff | OpPowerset

instance Show (SetOp a) where
    show OpSum = "+"
    show OpUnion = "&"
    show OpDiff = "-"
    show OpPowerset = "pow"

instance Read (SetOp a) where
    readsPrec _ = P.readP_to_S readP

instance Readable (SetOp a) where
    readP = readSum <|> readUnion <|> readDiff <|> readPowerset
        where
            readSum = (spaces >> P.char '+' >> spaces) $> OpSum
            readUnion = (spaces >> P.char '&' >> spaces) $> OpUnion
            readDiff = (spaces >> P.char '-' >> spaces) $> OpDiff
            readPowerset = (spaces >> P.string "pow" >> spaces) $>  OpPowerset

instance (Num a, Ord a) => (DAO (Op c (NSet a))) (SetOp a) where
    toValue OpSum = (+)
    toValue OpUnion = (&)
    toValue OpDiff = (-)
    toValue OpPowerset = pow