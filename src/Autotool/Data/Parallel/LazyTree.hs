module Autotool.Data.Parallel.LazyTree ( treesP, findTreeP, findTreePLim ) where

import Autotool.Data.LazyTree (evalTree,  Tree, Op, isOp0, isOp1, isOp2, trees )
import Data.List (find,  permutations, concatMap, )
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
treesP ops = concatMap factoryPar [0..]
    where
        nWorker = 6
        factoryPar n =
            let t1 = factory !! (n * nWorker)
                t2 = factory !! (n * nWorker + 1)
                t3 = factory !! (n * nWorker + 2)
                t4 = factory !! (n * nWorker + 3)
                t5 = factory !! (n * nWorker + 4)
                t6 = factory !! (n * nWorker + 5)
            in t1 `par` t2 `par` t3 `par` t4 `par` t5 `par` t6 `pseq` [t1, t2, t3, t4, t5, t6]
        factory = trees ops
-- treesP :: [Op a] -> [Tree (Op a)]
-- treesP ops = concatMap (sequence tss) [0..]
--     where
--         tss = map tree opss
--         op0s = filter isOp0 ops
--         op1s = filter isOp1 ops
--         op2s = filter isOp2 ops
--         opns = [op1s, op2s]
--         opss = map ((++op0s) . concat) $ mapM permutations opns

-- | Find a tree that evaluates to a given value by running all possible tree factories in parallel
--
-- see `Autotool.Data.LazyTree.findTree`
findTreeP :: (Eq a) =>
    [Op a]              -- ^ the operations defining the tree type
    -> a                -- ^ the target value to match against
    -> Tree (Op a)      -- ^ a tree that evaluates to `a` under `f`
findTreeP ops t = head $ filter ((==t) . evalTree) factory
    where
        factory = treesP ops

-- | Find a tree that evaluates to a given value by running all possible tree factories in parallel
--
-- see `Autotool.Data.LazyTree.findTree`
findTreePLim :: (Eq a) =>
    Int                      -- ^ limit of trees to create
    -> [Op a]                      -- ^ the operations defining the tree type
    -> a                        -- ^ the target value to match against
    -> Maybe (Tree (Op a))      -- ^ a tree that evaluates to `a` under `f`
findTreePLim lim ops t = find ((==t) . evalTree) (take lim factory)
    where
        factory = treesP ops

parMap :: (a -> b) -> [a] -> [b]
parMap _ [] = []
parMap f [x] = [f x]
parMap f (x:y:xs) = let x' = f x; y' = f y in x' `par` (y' `pseq` x':y':parMap f xs)