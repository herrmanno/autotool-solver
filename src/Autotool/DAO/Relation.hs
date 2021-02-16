{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Relation (RelOp) where

import Prelude hiding ((+), (-), (&), (*))
import Data.Set ( Set )
import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..), spaces)
import Autotool.Data.RelOp ((+), (-), (&), (*), inverse, reflexiveClosure, transitiveClosure)
import Autotool.Data.LazyTree (Op)

{- Relation OPERATIONS -}

data RelOp a
    = OpSum
    | OpUnion
    | OpDiff
    | OpCompose
    | OpInverse
    | OpReflexiveClosure
    | OpTransitiveClosure

instance Show (RelOp a) where
    show OpSum = "+"
    show OpUnion = "&"
    show OpDiff = "-"
    show OpCompose = "."
    show OpInverse = "inverse"
    show OpReflexiveClosure = "reflexive_cl"
    show OpTransitiveClosure = "transitive_cl"

instance Read (RelOp a) where
    readsPrec _ = P.readP_to_S readP

instance Readable (RelOp a) where
    readP = readSum <|> readUnion <|> readDiff <|> readCompose <|> readInverse <|> readReflexiveCl <|> readTransitivecl
        where
            readSum = (spaces >> P.char '+' >> spaces) $> OpSum
            readUnion = (spaces >> P.char '&' >> spaces) $> OpUnion
            readDiff = (spaces >> P.char '-' >> spaces) $> OpDiff
            readCompose = (spaces >> P.string "." >> spaces) $>  OpCompose
            readInverse = (spaces >> P.string "inverse" >> spaces) $>  OpInverse
            readReflexiveCl = (spaces >> P.string "reflexive_cl" >> spaces) $>  OpReflexiveClosure
            readTransitivecl = (spaces >> P.string "transitive_cl" >> spaces) $>  OpTransitiveClosure

instance (Num a, Ord a) => (DAO (Op [a] (Set (a,a)))) (RelOp a) where
    toValue OpSum = (+)
    toValue OpUnion = (&)
    toValue OpDiff = (-)
    toValue OpCompose = (*)
    toValue OpInverse = inverse
    toValue OpReflexiveClosure = reflexiveClosure
    toValue OpTransitiveClosure = transitiveClosure