{-# LANGUAGE LambdaCase #-}
module Autotool.Solver.GraphParam (solve) where

import qualified Data.Set as S
import Autotool.Data.Graph ( Graph, GraphConstraint(Edge, Not), satisfiesConstraint, insertEdge, breaksConstraint )
import Data.Maybe (mapMaybe, listToMaybe)


solve :: (Eq a, Ord a, Show a)
      => [GraphConstraint  a]   -- ^ constraints
      -> [a]                    -- ^ Universe
      -> Maybe (Graph a)
solve = findGraph

findGraph :: (Eq a, Ord a, Show a)
      => [GraphConstraint  a]   -- ^ constraints
      -> [a]                    -- ^ Universe
      -> Maybe (Graph a)
findGraph cs u = let (g, ess, cs') = buildBasicGraph cs u in listToMaybe $ go [g] ess cs'
    where
        go gs ess cs = do
            g <- gs
            e <- ess
            let g' = insertEdge g e
            let ess' = filter (/=e) ess
            if satisfiesAll g'
                then [g']
                else if breaksSome g'
                    then []
                    else go [g'] ess' cs
            
        satisfiesAll g = all (`satisfiesConstraint` g) cs
        breaksSome g = any (`breaksConstraint` g) cs

buildBasicGraph :: (Eq a, Ord a, Show a)
      => [GraphConstraint  a]
      -> [a]    -- ^ Universe
      -> (Graph a, [(a,a)], [GraphConstraint  a])
buildBasicGraph cs u =
    let
        g@(vs, es) = (S.fromList u, S.empty)
        ess = edgePairs u
        edgeConstraints = mapMaybe (\case { (Edge a b) -> Just (a,b); _ -> Nothing }) cs
        g' = (vs, foldr S.insert es edgeConstraints)
        ess'  = filter (`notElem` edgeConstraints) ess
        cs' = filter (\case { (Edge a b) -> False; _ -> True }) cs
        notEdgeConstraints = mapMaybe (\case { (Not (Edge a b)) -> Just (a,b); _ -> Nothing }) cs
        ess''  = filter (`notElem` notEdgeConstraints) ess
        cs'' = filter (\case { (Not(Edge a b)) -> False; _ -> True }) cs
    in (g',ess'',cs'')

edgePairs vs = let zs = zip [1..] vs in [(a,b) | (i,a) <- zs, (_,b) <- drop i zs]
