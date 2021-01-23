module Autotool.Solver.Circle (solve) where

import Data.Set (delete, toList, member, empty, size)
import Control.Monad (guard)
import Data.List (notElem, permutations, permutations, subsequences)
import Autotool.Data.Graph (Graph, mkGraph, kante, subgraph, isCircle)


solve :: (Eq a, Ord a) => Int -> Graph a -> [[a]]
solve n g@(vs,es) = map (toList . fst) $ filter isCircle subgraphs
    where subgraphs = map (subgraph g) $ filter ((==n) . length) $ subsequences (toList vs)