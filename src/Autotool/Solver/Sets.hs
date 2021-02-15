module Autotool.Solver.Sets (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.NestedSet ( NSet )
import Autotool.TreeSearch (SearchMode, searchTree, Tree, Op)

type NSetOp a = Op () (NSet a)
type NSetTree a = Tree (NSetOp a)

solve :: (Num a, Eq a, Show a)
    => SearchMode       -- ^ the search mode
    -> [NSetOp a]       -- ^ operators and constants
    -> NSet a           -- ^ result set
    -> NSetTree a
solve m ops t = case searchTree m ops () (==t) of
    (Just result) -> result
    _ -> error "No matching tree found"