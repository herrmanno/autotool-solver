module Autotool.Data.RelOp ((+), (&), (-), (*)) where

import Prelude hiding ((+), (-), (*), filter, map)

import Data.Set (Set, union, difference, intersection, powerSet, foldl', empty, filter, map, insert )
import Autotool.Data.LazyTree ( Op, mkOp2 )
import Control.Arrow (Arrow(first))

(+) :: (Ord a) => Op c (Set (a,a))
(+) = mkOp2 "+" True union

(&) :: (Ord a) => Op c (Set (a,a))
(&) = mkOp2 "&" True intersection

(-) :: (Ord a) => Op c (Set (a,a))
(-) = mkOp2 "-" False difference

(*) :: (Ord a) => Op c (Set (a,a))
(*) = mkOp2 "." False compose

-- TODO: this should be defined in its own file Data.Relation
compose :: (Ord a) => Set (a,a) -> Set (a,a) -> Set (a,a)
compose a b = foldl' f f0 a where
    f0 = empty :: Set (a,a)
    f acc (u,v) = let
        b' = map (first (const u)) $ filter ((==v) . fst) b
        in acc `union` b'