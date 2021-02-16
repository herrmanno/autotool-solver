module Autotool.Data.SetOp ((+), (&), (-), pow) where

import Prelude hiding ((+), (-))

import Data.Set (union, difference, intersection,)
import Autotool.Data.NestedSet (NSet, powerSet)
import Autotool.Data.LazyTree ( Op, mkOp1, mkOp2 )

(+) :: (Ord a) => Op c (NSet a)
(+) = mkOp2 "+" True union

(&) :: (Ord a) => Op c (NSet a)
(&) = mkOp2 "&" True intersection

(-) :: (Ord a) => Op c (NSet a)
(-) = mkOp2 "-" False difference

pow :: (Ord a) => Op c (NSet a)
pow = mkOp1 "pow" powerSet