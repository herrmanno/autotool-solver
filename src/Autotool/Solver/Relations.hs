module Autotool.Solver.Relations (solve, solveP) where

import Data.List (sortOn)
import Data.Function (on)
import Data.Set ( Set )
import Autotool.Data.LazyTree ( Tree, Op, findTreeLim )
import Autotool.Data.Parallel.LazyTree (searchTreeLimP)

type Rel a = Set (a,a)
type RelOp a = Op [a] (Rel a)

solve :: (Num a, Eq a, Show a) =>
    [RelOp a]       -- ^ operators and constants
    -> [a]          -- ^ the universe
    -> Rel a        -- ^ result set
    -> Tree (RelOp a)
solve ops u t = case findTreeLim lim ops u t of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 300000

solveP :: (Num a, Eq a, Show a) =>
    [RelOp a]       -- ^ operators and constants
    -> [a]          -- ^ the universe
    -> Rel a        -- ^ result set
    -> Tree (RelOp a)
solveP ops u t = case searchTreeLimP lim ops u (==t) of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 300000