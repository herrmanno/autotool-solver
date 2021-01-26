module Autotool.Solver.Relations (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Data.Set ( Set )
import Autotool.Data.LazyTree ( Tree, Op(Op0), findTreeLim )

type Rel a = Set (a,a)
type RelOp a = Op (Rel a)

solve :: (Num a, Eq a, Show a) =>
    [RelOp a]      -- ^ operators and constants
    -> Rel a       -- ^ result set
    -> Tree (RelOp a)
solve ops t = case findTreeLim lim ops t of
    (Just result) -> result
    _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"
    where
        lim = 300000