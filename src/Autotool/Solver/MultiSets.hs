module Autotool.Solver.MultiSets (solve) where

import Data.List (sortOn)
import Data.Function (on)
import Autotool.Data.MultiSet ( MultiSet )
import Autotool.Data.MultiSetOp ( MultiSetOp )
import Autotool.Data.LazyTree ( Tree, Op, findTreeLim, searchTreeLim )
import Autotool.Data.Parallel.LazyTree (searchTreeLimP)
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