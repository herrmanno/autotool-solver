module Autotool.Data.Parallel.LazyTree ( treesP, findTreeP ) where

import Autotool.Data.LazyTree (evalTree,  Tree, Op, isOp0, isOp1, isOp2, tree )
import Data.List ( permutations, concatMap, )
import Control.Parallel ( pseq, par )

-- |Generates an infinite list of trees from the given operator set
-- 
-- Does so by running x tree factory in parallel, where each factory differs
-- in the order of operator w/ rank > 0
-- 
-- example `treeP [+,*,-,pow, …]`
-- where `+`,`*` are rank-2 operator, `-`, `pow` are rank-1 operators
-- creates 4 trees with the operator orders
-- - `[-,pow,+,*]`
-- - `[-,pow,*,+]`
-- - `[pow,-,+,*]`
-- - `[pow,-,*,+]`
--
-- See `Autotool.Data.LazyTree.trees` for more information
treesP :: [Op a] -> [Tree (Op a)]
treesP ops = concatMap (sequence tss) [0..]
    where
        tss = map tree opss
        op0s = filter isOp0 ops
        op1s = filter isOp1 ops
        op2s = filter isOp2 ops
        opns = [op1s, op2s]
        opss = map ((++op0s) . concat) $ mapM permutations opns

-- | Find a tree that evaluates to a given value by running all possible tree factories in parallel
--
-- see `findTreeP`
findTreeP :: (Eq a) => [Op a]                -- ^ the operations defining the tree type
                   -> a                     -- ^ the target value to match against
                   -> Tree (Op a)           -- ^ a tree that evaluates to `a` under `f`
findTreeP ops t = head $ filter ((==t) . evalTree) factory
    where
        factory = treesP ops

parMap :: (a -> b) -> [a] -> [b]
parMap _ [] = []
parMap f [x] = [f x]
parMap f (x:y:xs) = let x' = f x; y' = f y in x' `par` (y' `pseq` x':y':parMap f xs)