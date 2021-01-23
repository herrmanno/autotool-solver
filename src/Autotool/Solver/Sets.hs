{-# LANGUAGE BangPatterns #-}

module Autotool.Solver.Sets (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.Set (S(..), insert, union, diff, intersect, pow)
import Autotool.Data.Op (Op2(..), Op1(..))
import Autotool.Data.Tree (Tree(..), size, buildTrees)


evalTree :: (Eq a, Show a) => Tree a -> S a
evalTree (Node0 !a) = a
evalTree (Node1 Pow !a) = pow $ evalTree a
evalTree (Node2 Add !a !b) = evalTree a `union` evalTree b
evalTree (Node2 Subtr !a !b) = evalTree a `diff` evalTree b
evalTree (Node2 And !a !b) = evalTree a `intersect` evalTree b

solve :: (Num a, Eq a, Show a) =>
    [Op2]       -- ^ 2-arity operators
    -> [Op1]    -- ^ 1-arity operators
    -> [S a]    -- ^ given sets
    -> S a      -- ^ result set
    -> Int      -- ^ max depth of result trees
    -> Tree a
solve op2s op1s ss r d = head $ sortOn size v where
    v = filter ((==r) . evalTree) ts
    ts = buildTrees op2s op1s ss d

-- main = do
--     let a = Set "A" [ V 1, V 2 ]
--     let b = Set "B" [ S [V 3] ]
--     --  {{}, {1, 2, {3}}, {1, {3}}, {2, {3}}, {{3}}}
--     let r = S[ S[], S[V 1, V 2, S [V 3]], S[V 1, S[V 3]], S[V 2, S[V 3]], S[S[V 3]] ]
--     let op2s = [Add, And, Subtr]
--     let op1s = [Pow]
--     print $ solve op2s op1s [a,b] r 3
