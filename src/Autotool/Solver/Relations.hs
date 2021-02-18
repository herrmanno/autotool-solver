module Autotool.Solver.Relations (solve) where

import Data.Set ( Set )
import Autotool.TreeSearch (SearchMode,Tree, Op, searchTree)
import Autotool.Data.RelOp (RelOpContext)

type Rel a = Set (a,a)
type RelOp c a = Op c (Rel a)

solve :: (Num a, Eq a, Show a, (RelOpContext a) c)
    => SearchMode
    -> [RelOp c a]       -- ^ operators and constants
    -> c          -- ^ the universe
    -> Rel a        -- ^ result set
    -> Tree (RelOp c a)
solve m ops c t = case searchTree m ops c (==t) of
    (Just result) -> result
    _ -> error "No matching tree found"
