module Autotool.Solver.Graphs (solve) where

import Prelude hiding ((+))
import Autotool.Data.GraphOp ((+))
import Autotool.Data.Graph ( Graph, vertices, normalize, similiar, disconnectedSubgraphs )
import Autotool.Data.LazyTree (evalTree', showTree, Tree, Op, mkOp0)
import Autotool.TreeSearch (SearchMode, searchTreeUnevaluated, searchTree)


solve :: (Integral a, Show a)
    => SearchMode
    -> [Op () (Graph a)]
    -> Graph a
    -> Tree (Op () (Graph a))
solve m ops t =
    let base = minimum (vertices t)
        gs = map (normalize 1) (disconnectedSubgraphs t)
        resultTrees = map (solveSubtree m ops . fst) gs
        resultGraphs = zipWith ($) (map snd gs) (map (evalTree' ()) resultTrees)
        resultOps = zipWith (mkOp0 . showTree) resultTrees resultGraphs
        result = searchTree m ((+):resultOps) () (similiar t)
    in case result of
        (Just r) -> r
        _ -> error "No expression found"

solveSubtree :: (Eq a, Show a, Ord a) => SearchMode -> [Op () (Graph a)] -> Graph a -> Tree (Op () (Graph a))
solveSubtree m ops t = case searchTreeUnevaluated m  ops p of
    (Just result) -> result
    _ -> error $ "No expression matching subtree '" ++ show t ++ "' found"
    where
        p = similiar t . evalTree' ()
