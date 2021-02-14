module Autotool.Solver.MultiSets (solve, solveP) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.MultiSet ( MultiSet )
import Autotool.Data.MultiSetOp ( MultiSetOp )
import Autotool.Data.LazyTree ( Tree, Op, findTreeLim, searchTreeLim )
import Autotool.Data.Parallel.LazyTree (searchTreeLimP)

type MultiSetTree a = Tree (MultiSetOp () a)

solve :: (Eq a, Show a) =>
    [MultiSetOp () a]       -- ^ operators and constants
    -> MultiSet a           -- ^ result set
    -> MultiSetTree a
solve ops t = case searchTreeLim lim ops () (==t) of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 500000

solveP :: (Eq a, Show a) =>
    [MultiSetOp () a]       -- ^ operators and constants
    -> MultiSet a           -- ^ result set
    -> MultiSetTree a
solveP ops t = case searchTreeLimP lim ops () (==t) of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 1000000