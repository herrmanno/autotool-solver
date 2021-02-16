module Autotool.Solver.Circle (solve) where

import Data.Set (toList)
import Data.List (subsequences)
import Autotool.Data.Graph (Graph, subgraph, isCircle)


solve :: (Eq a, Ord a) => Int -> Graph a -> [[a]]
solve n g@(vs,es) = map (toList . fst) $ filter isCircle subgraphs
    where subgraphs = map (subgraph g) $ filter ((==n) . length) $ subsequences (toList vs)