{-# LANGUAGE FlexibleInstances #-}
module Autotool.Data.Parallel.LazyTree (searchTreeLimP, searchTreeUnevaluatedLimP) where

import Autotool.Data.LazyTree (evalTree', Tree, Op, isOp0, searchTreeUnevaluatedLim )
import Data.List (permutations, )
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.DeepSeq (NFData(..))
import Control.Monad (msum)

instance {-# OVERLAPS #-} (Show a) => NFData (Tree (Op c a)) where
    rnf t = evalTree' t `seq` ()

searchTreeLimP :: (Show a, Eq a)
    => Int                      -- ^ the max number of trees to test
    -> [Op c a]                 -- ^ the operations defining the tree type
    -> c                        -- ^ the evaluation context
    -> (a -> Bool)              -- ^ the predicate to test against
    -> Maybe (Tree (Op c a))    -- ^ a tree that evaluates to `a` under `f`
searchTreeLimP lim ops c t = searchTreeUnevaluatedLimP lim ops t'
    where t' = t . evalTree' c

searchTreeUnevaluatedLimP :: (Show a, Eq a)
    => Int                      -- ^ the max number of trees to test
    -> [Op c a]                 -- ^ the operations defining the tree type
    -> (Tree (Op c a) -> Bool)  -- ^ the predicate to test against
    -> Maybe (Tree (Op c a))    -- ^ a tree that evaluates to `a` under `f`
searchTreeUnevaluatedLimP lim ops t =
    let
        op0s = filter isOp0 ops
        fnOps = filter (not . isOp0) ops
        opPerms = map (op0s ++) $ permutations fnOps
        search = \ops' -> searchTreeUnevaluatedLim lim ops' t
        ls = map search opPerms
        lspar = ls `using` parList rdeepseq
    in msum lspar