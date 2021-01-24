{-# LANGUAGE RankNTypes #-}

module Autotool.Data.LazyTree
    ( Tree(Node)
    , Op(..)
    , isOp0
    , isOp1
    , isOp2
    , findTree
    , showTree
    , showTreeFn
    , evalTree
    , tree
    , trees
    , termsLength
    , treesLevelCount
    , depth
    , size
    ) where

import Data.Tree (foldTree, levels, Tree(Node) )
import Debug.Trace (traceShow, traceShowId)

data Op a = Op0 a | Op1 String (a -> a) | Op2 String (a -> a -> a)

isOp0 (Op0 _) = True
isOp0 _ = False

isOp1 (Op1 _ _) = True
isOp1 _ = False

isOp2 (Op2 _ _) = True
isOp2 _ = False

eval :: Op a -> [a] -> a
eval (Op0 a) _ = a
eval (Op1 _ f) [a] = f a
eval (Op2 _ f) [a,b] = f a b

evalTree = foldTree eval

instance (Eq a) => Eq (Op a) where
    (Op0 a) == (Op0 b) = a == b
    (Op1 a _) == (Op1 b _) = a == b
    (Op2 a _) == (Op2 b _) = a == b
    _ == _ = False

instance (Show a) => Show (Op a) where
    show (Op0 a) = show a
    show (Op1 s _) = s
    show (Op2 s _) = s

-- | Find a tree that evaluates to a given value
findTree :: (Eq a) => [Op a]                -- ^ the operations defining the tree type
                   -> a                     -- ^ the target value to match against
                   -> Tree (Op a)           -- ^ a tree that evaluates to `a` under `f`
findTree ops t = head $ filter ((==t) . evalTree) factory
    where
        factory = trees ops

-- |Generates an infinite list of trees from the given operator set
-- See `tree` for more information
trees :: [Op a] -> [Tree (Op a)]
trees ops = map factory [0..] where factory = tree ops

-- |Lazy-builds a list of trees from a given operator and takes a specific one from the list
-- 
-- example: build arithmetic trees from a given set of constants (1,2)
--
-- >>> tree [Op0 1, Op0 2, Op1, "-" negate, Op2 "+" (+)] 0
--
tree ::  [Op a]         -- ^ the given operators
      -> Int            -- ^ the tree index to retrieve
      -> Tree (Op a)    -- ^ the n-th tree
tree ops = (fmap f [0..] !!) where
    -- constants
    ts = tree ops
    ar0 = filter isOp0 ops
    ar1 = filter isOp1 ops
    ar2 = filter isOp2 ops
    -- functions
    f n
        | n < length ar0 = Node (ar0 !! n) []
        | otherwise = let op = operator n in Node op $ children op n
    children op n = let tl = termsL n; tl2 = tl^2 in case op of
        (Op1 _ _) -> [ts (n - tl)]
        (Op2 _ _) -> [ts (n `rem` tl2 `div` tl), ts (n `rem` tl2 `rem` tl)]
        _ -> []
    operator n =
        let tl = termsL n
            ops = concatMap (replicate tl) ar1 ++ concatMap (replicate (tl^2)) ar2
        in ops !! (n - tl)
    termsL = termsLength (map length [ar0, ar1, ar2])

-- |Returns the number of unique trees w/ given operators and constants of a specific depth
--
-- >>> treesLevelCount [2, 3, 4] 3
-- 16038022
-- 
-- returns the number of unique trees of depth 3
-- build from a set of
--  - 2 constants
--  - 3 1-arity functions
--  - 4 2-arity functions
treesLevelCount :: [Int] -> Int -> Int
treesLevelCount ns 0 = head ns
treesLevelCount ns d =
    let n_1 = treesLevelCount ns (d - 1)
    in sum $ zipWith (\nn idx -> nn * n_1^idx) (tail ns) [1..]

-- |Returns the number of unique terms for a tree at position *n* in a lazy tree list
-- example:
-- 
-- >>> termsLength [2,1,1] 10
-- 8
--
-- returns the number of terms for a depth 3 (from n == 10) tree build upon
-- - 2 constants
-- - 1 1-arity function
-- - 1 2-arity function
termsLength :: [Int] -> Int -> Int
termsLength ns n
    | n < head ns = head ns
    | otherwise = last $ takeWhile (<=n) $ scanl1 (+) $ map (treesLevelCount ns) [0..]

depth :: Tree (Op a) -> Int
depth = length . levels

size :: Tree (Op a) -> Int
size (Node _ xs) = 1 + sum (map size xs)

showTree :: (Show a) => Tree (Op a) -> String
showTree = showTreeFn show

showTreeFn :: (Show a) => (a -> String) -> Tree (Op a) -> String
showTreeFn showValue = foldTree f
    where
        f (Op0 a) _ = showValue a
        f (Op1 s _) [a] =  s ++ "(" ++ a ++ ")"
        f (Op2 s _) [a,b] = "(" ++ a ++ ") " ++ s ++ " (" ++ b ++ ")"