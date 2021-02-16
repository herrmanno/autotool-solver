module Autotool.Data.MultiSetOp (MultiSetOp, (+), (&), (-)) where

import Prelude hiding ((+), (-))

import Autotool.Data.MultiSet (MultiSet, disjointUnion, intersection, difference)
import Autotool.Data.LazyTree ( Op, mkOp2 )

type MultiSetOp c a = Op c (MultiSet a)

-- | Disjoint union
(+) :: (Ord a) => Op c (MultiSet a)
(+) = mkOp2 "+" True disjointUnion

-- |Intersection
(&) :: (Ord a) => Op c (MultiSet a)
(&) = mkOp2 "&" True intersection

-- | Difference
(-) :: (Ord a) => Op c (MultiSet a)
(-) = mkOp2 "-" False difference