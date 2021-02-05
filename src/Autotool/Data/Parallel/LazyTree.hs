{-# LANGUAGE FlexibleInstances #-}
module Autotool.Data.Parallel.LazyTree (searchTreeLimP) where

import Autotool.Data.LazyTree (evalTree', Tree, Op, isOp0, searchTree, searchTreeLim )
import Data.List (find,  permutations, concatMap, )
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.DeepSeq (NFData(..))
import Control.Monad (msum)

instance {-# OVERLAPS #-} (Show a) => NFData (Tree (Op c a)) where
    rnf t = evalTree' t `seq` ()

searchTreeLimP :: (Show a, Eq a)
    => Int
    -> [Op c a]                 -- ^ the operations defining the tree type
    -> c                        -- ^ the evaluation context
    -> (a -> Bool)              -- ^ the target value to match against
    -> Maybe (Tree (Op c a))    -- ^ a tree that evaluates to `a` under `f`
searchTreeLimP lim ops c t =
    let
        op0s = filter isOp0 ops
        fnOps = filter (not . isOp0) ops
        opPerms = map (op0s ++) $ permutations fnOps
        search = \ops' -> searchTreeLim lim ops' c t
        ls = map search opPerms
        lspar = ls `using` parList rdeepseq
    in msum lspar