module Autotool.Data.Relation (compose, inverse, reflexiveClosure, transitiveClosure) where

import Prelude hiding ((+), (-), (*), filter, map)

import Data.List (unfoldr)
import Data.Set (Set, fromList, union, unions, foldl', empty, filter, map )
import Control.Arrow (Arrow(first))

type Relation a = Set (a,a)

-- | Composes two relations
--
-- >>> compose (fromList [(1,2), (2,2)]) (fromList [(2,3)])
-- fromList [(1,3),(2,3)]
compose :: (Ord a) => Relation a -> Relation a -> Relation a
compose a b = foldl' f f0 a where
    f0 = empty :: Set (a,a)
    f acc (u,v) = let
        b' = map (first (const u)) $ filter ((==v) . fst) b
        in acc `union` b'


-- | Inverses each tuple of a relation
--
-- >>> inverse (fromList [(1,2), (2,2)])
-- fromList [(2,1),(2,2)]
inverse :: (Ord a) => Relation a -> Relation a
inverse = map (\(a,b) -> (b,a))

-- | Computes a relation's reflexive closure
--
-- >>> reflexiveClosure [1,2,3] (fromList [(1,2), (2,3)])
-- fromList [(1,1),(1,2),(2,2),(2,3),(3,3)]
reflexiveClosure :: (Ord a) => [a] -> Relation a -> Relation a
reflexiveClosure c r = r `union` fromList (zip c c)

-- | Computes a relation's transitive closure
--
-- >>> transitiveClosure (fromList [(1,2), (2,3)])
-- fromList [(1,2),(1,3),(2,3)]
--
-- >>> transitiveClosure (fromList [(1,2), (2,3), (3,1)])
-- fromList [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
transitiveClosure :: (Ord a) => Relation a -> Relation a
transitiveClosure r = unions (r : unfoldr mkTransitive r)
    where
        mkTransitive r' = let r'' = compose r' r
                          in if r'' == r' || null r''
                                then Nothing
                                else Just (r'',r'')
