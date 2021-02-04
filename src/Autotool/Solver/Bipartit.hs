module Autotool.Solver.Bipartit (solve) where

import Data.Set (delete, insert,  empty, toList )
import Autotool.Data.Graph
import Data.Foldable (find)


solve :: (Integral a, Show a)
      => Graph a    -- ^ the given input graph
      -> [a]        -- ^ a set of vertices forming a bipartit subgraph
solve g@(vs, es) = toList $ go g empty vs
    where
        go g xs ys
            | isEdgeless xs && isEdgeless ys = xs
            | otherwise = case unconnectedVertice xs ys of
                (Just v) -> go g (insert v xs) (delete v ys)
                _ -> error "Graph can not be reduced to bipartit subgraphs"
        unconnectedVertice xs ys = find (\y -> not $ any (\x -> connected g x y) xs) ys
        isEdgeless = null . edges . subgraph g . toList

