{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Autotool.Data.RelOp (RelOpContext(..), (+), (&), (-), (*), inverse, reflexiveClosure, transitiveClosure) where

import Prelude hiding ((+), (-), (*), filter, map)

import Data.Set (Set, union, difference, intersection )
import Autotool.Data.LazyTree ( Op, mkOp2, mkOp1, mkOp1C )
import qualified Autotool.Data.Relation  as R

class RelOpContext a c where
    universe :: c -> [a]

instance RelOpContext a [a] where
    universe = id

type RelOp c a = Op c (Set (a,a)) -- TODO: not really needed?

(+) :: (Ord a) => RelOp c a
(+) = mkOp2 "+" True union

(&) :: (Ord a) => RelOp c a
(&) = mkOp2 "&" True intersection

(-) :: (Ord a) => RelOp c a
(-) = mkOp2 "-" False difference

(*) :: (Ord a) => RelOp c a
(*) = mkOp2 "." False R.compose

inverse :: (Ord a) => RelOp c a
inverse = mkOp1 "inverse" R.inverse

reflexiveClosure :: (Ord a, RelOpContext a c) => RelOp c a
reflexiveClosure = mkOp1C "reflexive_cl" (R.reflexiveClosure . universe)

transitiveClosure :: (Ord a) => RelOp c a
transitiveClosure = mkOp1 "transitive_cl" R.transitiveClosure