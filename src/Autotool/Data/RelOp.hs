module Autotool.Data.RelOp ((+), (&), (-), (*)) where

import Prelude hiding ((+), (-), (*), filter, map)

import Data.Set (Set, union, difference, intersection, powerSet, foldl', empty, filter, map, insert )
import Autotool.Data.LazyTree ( Op(Op1, Op2) )
import Control.Arrow (Arrow(first))

(+) :: (Ord a) => Op (Set (a,a))
(+) = Op2 "+" True union

(&) :: (Ord a) => Op (Set (a,a))
(&) = Op2 "&" True intersection

(-) :: (Ord a) => Op (Set (a,a))
(-) = Op2 "-" False difference

(*) :: (Ord a) => Op (Set (a,a))
(*) = Op2 "." False compose

compose :: (Ord a) => Set (a,a) -> Set (a,a) -> Set (a,a)
compose a b = foldl' f f0 a where
    f0 = empty :: Set (a,a)
    f acc (u,v) = let
        b' = map (first (const u)) $ filter ((==v) . fst) b
        in acc `union` b'