module Autotool.Solver.Sets (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.NestedSet ( NSet )
import Autotool.Data.LazyTree ( Tree, Op(Op0) )
import Autotool.Data.Parallel.LazyTree ( findTreeP )

type NSetOp a = Op (NSet a)

solve :: (Num a, Eq a, Show a) =>
    [NSetOp a]      -- ^ operators and constants
    -> NSet a       -- ^ result set
    -> Tree (NSetOp a)
solve = findTreeP