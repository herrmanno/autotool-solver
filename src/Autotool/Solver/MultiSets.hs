module Autotool.Solver.MultiSets (solve) where

import Autotool.Data.MultiSet ( MultiSet )
import Autotool.Data.MultiSetOp ( MultiSetOp )
import Autotool.Data.LazyTree ( Tree )
import Autotool.TreeSearch (SearchMode, searchTree)

type MultiSetTree a = Tree (MultiSetOp () a)

solve :: (Eq a, Show a)
    => SearchMode
    -> [MultiSetOp () a]       -- ^ operators and constants
    -> MultiSet a           -- ^ result set
    -> MultiSetTree a
solve m ops t = case searchTree m ops () (==t) of
    (Just result) -> result
    _ -> error "No matching tree found"