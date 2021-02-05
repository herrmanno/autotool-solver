{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Relation (RelOp) where

import Prelude hiding ((+), (-), (&), (*))
import Data.Set ( Set )
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), spaces)
import Autotool.Data.RelOp ((+), (-), (&), (*))
import Autotool.Data.LazyTree (Op)

{- Relation OPERATIONS -}

data RelOp a = OpSum | OpUnion | OpDiff | OpCompose

instance Show (RelOp a) where
    show OpSum = "+"
    show OpUnion = "&"
    show OpDiff = "-"
    show OpCompose = "."

instance Read (RelOp a) where
    readsPrec _ = P.readP_to_S readP

instance Readable (RelOp a) where
    readP = readSum <|> readUnion <|> readDiff <|> readCompose
        where
            readSum = (spaces >> P.char '+' >> spaces) $> OpSum
            readUnion = (spaces >> P.char '&' >> spaces) $> OpUnion
            readDiff = (spaces >> P.char '-' >> spaces) $> OpDiff
            readCompose = (spaces >> P.string "." >> spaces) $>  OpCompose

instance (Num a, Ord a) => (DAO (Op c (Set (a,a)))) (RelOp a) where
    toValue OpSum = (+)
    toValue OpUnion = (&)
    toValue OpDiff = (-)
    toValue OpCompose = (*)