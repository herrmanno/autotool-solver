module Autotool.Solver.Relations (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Data.Set ( Set )
import Autotool.Data.LazyTree ( Tree, Op(Op0) )
import Autotool.Data.Parallel.LazyTree ( findTreeP )

type Rel a = Set (a,a)
type RelOp a = Op (Rel a)

solve :: (Num a, Eq a, Show a) =>
    [RelOp a]      -- ^ operators and constants
    -> Rel a       -- ^ result set
    -> Tree (RelOp a)
solve = findTreeP