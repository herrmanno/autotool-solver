{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.Data.LazyTree
    ( Tree(Node)
    , Op(..)
    , isOp0
    , isOp1
    , isOp2
    , findTree
    , findTreeLim
    , searchTree
    , searchTreeLim
    , showTree
    , showFnTree
    , evalTree
    , evalTree'
    , eval
    , trees
    , depth
    , size
    ) where

import Data.Tree (foldTree, levels, Tree(Node) )
import Debug.Trace (traceShow, traceShowId)
import Data.List (sort, subsequences, find, notElem, )

data Op a = Op0 String a | Op1 String (a -> a) | Op2 String Bool (a -> a -> a)

isOp0 :: Op a -> Bool
isOp0 Op0 {} = True
isOp0 _ = False

isOp1 :: Op a -> Bool
isOp1 Op1 {} = True
isOp1 _ = False

isOp2 :: Op a -> Bool
isOp2 Op2 {} = True
isOp2 _ = False

-- | Matches commutative 2-arity operators
isCommutative :: Op a -> Bool
isCommutative (Op2 _ c _) = c
isCommutative _ = False

-- | Matches non-commutative 2-arity operators
isNonCommutative :: Op a -> Bool
isNonCommutative (Op2 _ c _) = not c
isNonCommutative _ = False

-- | Evals an operator with the given arguments
eval :: Op a -> [a] -> a
eval (Op0 _ a) _ = a
eval (Op1 _ f) [a] = f a
eval (Op2 _ _ f) [a,b] = f a b

-- | Strict version of `eval`
eval' :: Op a -> [a] -> a
eval' (Op0 _ !a) _ = a
eval' (Op1 _ f) [!a] = let r = f a in r
eval' (Op2 _ _ f) [!a,!b] = let r = f a b in r

-- | Recursevely evaluates a operator tree
evalTree :: Tree (Op b) -> b
evalTree = foldTree eval

-- | Strict version of `evalTree`
evalTree' :: Tree (Op b) -> b
evalTree' = foldTree eval'

instance (Eq a) => Eq (Op a) where
    (Op0 _ a) == (Op0 _ b) = a == b
    (Op1 a _) == (Op1 b _) = a == b
    (Op2 a _ _) == (Op2 b _ _) = a == b
    _ == _ = False

instance Show (Op a) where
    show (Op0 s _) = s
    show (Op1 s _) = s
    show (Op2 s _ _) = s

-- | Find a tree that evaluates to a given value
findTree :: (Eq a) => [Op a]                -- ^ the operations defining the tree type
                   -> a                     -- ^ the target value to match against
                   -> Tree (Op a)           -- ^ a tree that evaluates to `a` under `f`
findTree ops t = searchTree ops (==t)

-- | Find a tree that satifies a given predicate when evaluated
searchTree :: (Eq a) => [Op a]              -- ^ the operations defining the tree type
                   -> (a -> Bool)             -- ^ the predicate the trees evaluation must satisfy
                   -> Tree (Op a)           -- ^ a tree that evaluates to `a` under `f`
searchTree ops p = head $ filter (p . evalTree') factory
    where
        factory = trees ops

-- | Find a tree that evaluates to a given value by creating at most `lim` trees
--
-- see `Autotool.Data.LazyTree.findTree`
findTreeLim :: (Eq a) =>
    Int                      -- ^ limit of trees to create
    -> [Op a]                      -- ^ the operations defining the tree type
    -> a                        -- ^ the target value to match against
    -> Maybe (Tree (Op a))      -- ^ a tree that evaluates to `a` under `f`
findTreeLim lim ops t = searchTreeLim lim ops (==t)

-- | see `Autotool.Data.LazyTree.searchTree`
searchTreeLim :: (Eq a) =>
    Int                         -- ^ limit of trees to create
    -> [Op a]                   -- ^ the operations defining the tree type
    -> (a -> Bool)              -- ^ the target value to match against
    -> Maybe (Tree (Op a))      -- ^ a tree that evaluates to `a` under `f`
searchTreeLim lim ops p = find (p . evalTree') (take lim factory)
    where
        factory = trees ops

{- | Create an (infinite) list of all possible trees under a given set of operators and constants
    
    It works be cycling repeatedly through individual _runs_, where a run
    consists of multiple operator runs (one for each given non-const operator).

    An operator run consists of building _n_ trees w/ one operator as tree root where
    the number of trees to build in this run is determined at the start of the run.
 -}
trees :: forall a. [Op a]       -- ^ the given operators
      -> [Tree (Op a)]          -- ^ the list of trees
trees ops = map (`Node` []) consts ++ go [] [] (length consts) 0 0
    where
        ts = (!!) (trees ops)
        -- | constructs trees lazy
        -- It works be cycling repeatedly through individual _runs_, where a run
        -- consists of multiple operator runs (one for each given non-const operator).
        -- 
        -- An operator run consists of 'building n trees w/ one operator as tree root' where
        -- the number of trees to build in this run is determined at the start of the run
        -- (see 'buildValues').
        go :: [Op a]            -- ^ operators - the next tree will be constructed with first op from this list
           -> [[Tree (Op a)]]   -- ^ values - the next tree will be construction with the first [values] from this list
           -> Int               -- ^ the current tree index
           -> Int               -- ^ the index where the current run started at
           -> Int               -- ^ the index where the previous run started at
           -> [Tree (Op a)]
        go [] _ i n m =                     -- end of last operator run
            let os = fnOps
                values = buildValues (head os) i n
            in go os values i i n
        go (_:os) [] i n m =                -- end of operator run
            let values = buildValues (head os) n m
            in go os values i n m
        go os (v:values) i n m =            -- during operator run
            Node (head os) v : go os values (i + 1) n m
        buildValues op n m
            | isOp1 op = map ((:[]) . ts) [m..n-1]
            | isNonCommutative op = combinations $ map ts [0..n-1]
            | isCommutative op = do
                a <- [0..n-1]
                b <- [max m a .. n - 1]
                return [ts a, ts b]
        fnOps = filter (not . isOp0) ops
        consts = filter isOp0 ops

-- | builds all possible tuples (as list [a,b]) from a given list
combinations :: [a] -> [[a]]
combinations xs = do
    a <- xs
    b <- xs
    return [a, b]

depth :: Tree (Op a) -> Int
depth = length . levels

size :: Tree (Op a) -> Int
size (Node _ xs) = 1 + sum (map size xs)

showTree :: (Show a) => Tree (Op a) -> String
showTree = foldTree f
    where
        f (Op0 s _) _ = s
        f (Op1 s _) [a] =  s ++ "(" ++ a ++ ")"
        f (Op2 s _ _) [a,b] = brOp a ++ " " ++ s ++ " " ++ brOp b
            where
                brOp [c] = [c]
                brOp c = "(" ++ c ++ ")"

showFnTree :: (Show a) => Tree (Op a) -> String
showFnTree = foldTree f
    where
        f (Op0 s _) _ = s ++ "()"
        f (Op1 s _) [a] =  s ++ "(" ++ a ++ ")"
        f (Op2 s _ _) [a,b] = s ++ "(" ++ a ++ "," ++ b ++ ")"
            where
                brOp [c] = [c]
                brOp c = "(" ++ c ++ ")"