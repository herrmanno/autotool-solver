module Autotool.Data.RelOp ((+), (&), (-), (*), inverse, reflexiveClosure, transitiveClosure) where

import Prelude hiding ((+), (-), (*), filter, map)

import Data.Set (Set, union, difference, intersection )
import Autotool.Data.LazyTree ( Op, mkOp2, mkOp1, mkOp1C )
import qualified Autotool.Data.Relation  as R

type RelOp a = Op [a] (Set (a,a))

(+) :: (Ord a) => RelOp a
(+) = mkOp2 "+" True union

(&) :: (Ord a) => RelOp a
(&) = mkOp2 "&" True intersection

(-) :: (Ord a) => RelOp a
(-) = mkOp2 "-" False difference

(*) :: (Ord a) => RelOp a
(*) = mkOp2 "." False R.compose

inverse :: (Ord a) => RelOp a
inverse = mkOp1 "inverse" R.inverse

reflexiveClosure :: (Ord a) => RelOp a
reflexiveClosure = mkOp1C "reflexive_cl" R.reflexiveClosure

transitiveClosure :: (Ord a) => RelOp a
transitiveClosure = mkOp1 "transitive_cl" R.transitiveClosure