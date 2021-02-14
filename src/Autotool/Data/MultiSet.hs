module Autotool.Data.MultiSet (MultiSet, disjointUnion, intersection, difference) where

import Data.Map (Map)
import qualified Data.Map as M


type MultiSet a = Map a Int

-- | Disjoint union of two bags
--
-- >>> disjointUnion (M.fromList [('a', 1), ('b', 1)]) (M.fromList [('a', 2), ('c', 1)])
-- fromList [('a',3),('b',1),('c',1)]
disjointUnion :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
disjointUnion ma mb = M.filter (>0) $ M.unionWithKey (const (+)) ma mb

-- | Intersection of two bags
--
-- >>> intersection (M.fromList [('a', 1), ('b', 1)]) (M.fromList [('a', 2), ('c', 1)])
-- fromList [('a',1)]
intersection :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersection = M.intersectionWithKey (const min)

-- |Difference of two bags
--
-- >>> difference (M.fromList [('a', 1), ('b', 1)]) (M.fromList [('a', 2), ('c', 1)])
-- fromList [('b',1)]
-- >>> difference (M.fromList [('a', 2), ('b', 1)]) (M.fromList [('a', 1), ('c', 1)])
-- fromList [('a',1),('b',1)]
difference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
difference = M.differenceWithKey (const diff)
    where
        diff :: (Num a, Ord a) => a -> a -> Maybe a
        diff a b
            | a >= b = Just (a - b)
            | otherwise = Nothing
