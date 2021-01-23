{-# LANGUAGE BangPatterns #-}

module Autotool.Solver.Graphs (solve) where

import Data.List (sortOn)
import Control.Monad (guard)
import Autotool.Data.Set (S(..))
import Autotool.Data.Op (Op2(..), Op1(..))
import Autotool.Data.Graph (Graph, kante, mkGraph, mkK, mkP, mkC, complement, join, add)
import Autotool.Data.Tree (Tree(..), size, buildTrees)


evalTree :: (Eq a, Ord a, Num a, Show a) => Tree (Graph a) -> Graph a
evalTree (Node0 (V a)) = a
evalTree (Node1 Complement !a) = complement $ evalTree a
evalTree (Node2 Add !a !b) = evalTree a `add` evalTree b
evalTree (Node2 Junction !a !b) = evalTree a `join` evalTree b
-- TODO: define graph subtr and graph diff

solve :: (Eq a, Ord a, Num a, Show a) =>
       [Op2]            -- ^ 2-arity operators
    -> [Op1]            -- ^ 1-arity operators
    -> [S (Graph a)]    -- ^ given graphs
    -> Graph a          -- ^ result graph
    -> Int              -- ^ max depth of result trees
    -> Tree (Graph a)
solve op2s op1s ss r d = head $ sortOn size v where
    v = filter ((==r) . evalTree) ts
    ts = buildTrees op2s op1s ss d

-- main = do
--     let r = mkGraph [1..10] [ kante 0 6
--                        , kante 0 7
--                        , kante 0 8
--                        , kante 0 9
--                        , kante 1 6
--                        , kante 1 7
--                        , kante 1 8
--                        , kante 1 9
--                        , kante 2 6
--                        , kante 2 7
--                        , kante 2 8
--                        , kante 2 9
--                        , kante 3 6
--                        , kante 3 7
--                        , kante 3 8
--                        , kante 3 9
--                        , kante 4 6
--                        , kante 4 7
--                        , kante 4 8
--                        , kante 4 9
--                        , kante 5 6
--                        , kante 5 7
--                        , kante 5 8
--                        , kante 5 9
--                        , kante 6 8
--                        , kante 7 9
--                        ] 
--     let op2s = [Add,Junction]
--     let op1s = [Complement]
--     let op0s = map V (map mkK [1..5] ++ map mkP [3..5] ++ map mkC [3..5])
--     -- print $ solve op2s op1s op0s r 3
--     print $ length $ buildTrees op2s op1s op0s 3
