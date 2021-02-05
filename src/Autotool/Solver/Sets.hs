module Autotool.Solver.Sets (solve, solveP) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.NestedSet ( NSet )
import Autotool.Data.LazyTree ( Tree, Op(Op0), findTreeLim, searchTreeLim )
import Autotool.Data.Parallel.LazyTree (searchTreeLimP)

type NSetOp a = Op (NSet a)
type NSetTree a = Tree (NSetOp a)

solve :: (Num a, Eq a, Show a) =>
    [NSetOp a]      -- ^ operators and constants
    -> NSet a       -- ^ result set
    -> NSetTree a
solve ops t = case searchTreeLim lim ops (==t) of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 500000

solveP :: (Num a, Eq a, Show a) =>
    [NSetOp a]      -- ^ operators and constants
    -> NSet a       -- ^ result set
    -> NSetTree a
solveP ops t = case searchTreeLimP lim ops (==t) of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 100000