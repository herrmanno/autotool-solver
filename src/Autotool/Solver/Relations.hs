{-# LANGUAGE BangPatterns #-}

module Autotool.Solver.Relations (solve) where

import Data.List (sortOn)
import Control.Monad (guard)
import Autotool.Data.Set (S(..), insert, union, diff, intersect, compose, pow)
import Autotool.Data.Op (Op2(..), Op1(..))
import Autotool.Data.Tree (Tree(..), size, buildTrees)


evalTree :: (Eq a, Show a) => Tree (a,a) -> S (a,a)
evalTree (Node0 !a) = a
evalTree (Node1 Pow !a) = pow $ evalTree a
evalTree (Node2 Add !a !b) = evalTree a `union` evalTree b
evalTree (Node2 Subtr !a !b) = evalTree a `diff` evalTree b
evalTree (Node2 And !a !b) = evalTree a `intersect` evalTree b
evalTree (Node2 Compose !a !b) = evalTree a `compose` evalTree b

solve :: (Eq a, Show a)
    => [Op2]        -- ^ 2-arity operators
    -> [Op1]        -- ^ 1-arity operators
    -> [S (a,a)]    -- ^ given relations
    -> S (a,a)      -- ^ result relation
    -> Int          -- ^ max depth of result trees
    -> Tree (a,a)
solve op2s op1s ss r d = head $ sortOn size v where
    v = filter ((==r) . evalTree) ts
    ts = buildTrees op2s op1s ss d

-- main = do
--     let a = Set "R" [ V(1 , 4), V(2 , 4), V(3 , 2), V(4 , 1) ] :: S (Int,Int)
--     let b = Set "S" [ V(1 , 4), V(2 , 2), V(2 , 3), V(4 , 4) ]
--     let r = S[ V(1 , 1) , V(1 , 4) , V(2 , 1) , V(2 , 2) , V(2 , 4) , V(4 , 1) , V(4 , 4) ]
--     let op2s = [Add, And, Subtr, Compose]
--     let op1s = []
--     print $ solve op2s op1s [a,b] r 3
