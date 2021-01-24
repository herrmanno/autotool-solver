module Autotool.Data.SetOp ((+), (&), (-), pow) where

import Prelude hiding ((+), (-))

import Data.Set (Set, union, difference, intersection,)
import Autotool.Data.NestedSet (NSet, powerSet)
import Autotool.Data.LazyTree ( Op(Op1, Op2) )

(+) :: (Ord a) => Op (NSet a)
(+) = Op2 "+" union

(&) :: (Ord a) => Op (NSet a)
(&) = Op2 "&" intersection

(-) :: (Ord a) => Op (NSet a)
(-) = Op2 "-" difference

pow :: (Ord a) => Op (NSet a)
pow = Op1 "pow" powerSet