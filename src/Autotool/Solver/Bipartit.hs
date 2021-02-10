module Autotool.Solver.Bipartit (solve) where

import Data.Set (delete, insert,  empty, toList )
import Autotool.Data.Graph
import Data.Foldable (find)


solve :: (Integral a, Show a)
      => Graph a    -- ^ the given input graph
      -> Either String [a]        -- ^ a set of vertices forming a bipartit subgraph
solve g@(vs, es) = toList <$> go g empty vs
    where
        go g xs ys
            | isEdgeless xs && isEdgeless ys = Right xs
            | otherwise = case unconnectedVertice xs ys of
                (Just v) -> go g (insert v xs) (delete v ys)
                _ -> Left "Graph can not be reduced to bipartit subgraphs"
        unconnectedVertice xs ys = find (\y -> not $ any (\x -> connected g x y) xs) ys
        isEdgeless = null . edges . subgraph g . toList

