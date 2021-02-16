module Autotool.Solver.Relations (solve) where

import Data.Set ( Set )
import Autotool.TreeSearch (SearchMode,Tree, Op, searchTree)

type Rel a = Set (a,a)
type RelOp a = Op [a] (Rel a)

solve :: (Num a, Eq a, Show a)
    => SearchMode
    -> [RelOp a]       -- ^ operators and constants
    -> [a]          -- ^ the universe
    -> Rel a        -- ^ result set
    -> Tree (RelOp a)
solve m ops u t = case searchTree m ops u (==t) of
    (Just result) -> result
    _ -> error "No matching tree found"
