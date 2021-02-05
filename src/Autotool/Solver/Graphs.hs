module Autotool.Solver.Graphs (solve) where

import Prelude hiding ((+))
import Autotool.Data.GraphOp ((+))
import Autotool.Data.Graph ( Graph, vertices, normalize, similiar, disconnectedSubgraphs )
import Autotool.Data.LazyTree (searchTreeLim, evalTree, showTree, Tree, Op, mkOp0)


solve :: (Integral a, Show a) =>
       [Op () (Graph a)]
    -> Graph a
    -> Tree (Op () (Graph a))
solve ops t =
    let base = minimum (vertices t)
        gs = map (normalize 1) (disconnectedSubgraphs t)
        resultTrees = map (solveSubtree ops . fst) gs
        resultGraphs = zipWith ($) (map snd gs) (map (evalTree ()) resultTrees)
        resultOps = zipWith (mkOp0 . showTree) resultTrees resultGraphs
        result = searchTreeLim lim ((+):resultOps) () (similiar t)
    in case result of
        (Just r) -> r
        _ -> error $ "No expression foudn within " ++ show lim ++ " candidates"
    where lim = 100000

solveSubtree :: (Eq a, Show a) => [Op () (Graph a)] -> Graph a -> Tree (Op () (Graph a))
solveSubtree ops t = case searchTreeLim lim ops () (similiar t) of
    (Just result) -> result
    _ -> error $ "No expression matching tree '" ++ show t ++ "' found within first " ++ show lim ++ " candidates"
    where
        lim = 300000
