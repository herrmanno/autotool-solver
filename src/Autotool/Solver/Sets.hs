module Autotool.Solver.Sets (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.NestedSet ( NSet )
import Autotool.Data.LazyTree ( Tree, Op(Op0), findTreeLim )

type NSetOp a = Op (NSet a)

solve :: (Num a, Eq a, Show a) =>
    [NSetOp a]      -- ^ operators and constants
    -> NSet a       -- ^ result set
    -> Tree (NSetOp a)
solve ops t = case findTreeLim lim ops t of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 300000