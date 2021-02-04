module Autotool.Solver.Isomorphism (solve) where

import Autotool.Data.Graph ( findIsomorphism, Graph, Isomorphism )


solve :: (Ord a, Ord b)
      => Graph a                    -- ^ the source graph 'g'
      -> Graph b                    -- ^ the destination graph 'h'
      -> Maybe (Isomorphism a b)    -- ^ an isomorphism from g to h
solve = findIsomorphism

