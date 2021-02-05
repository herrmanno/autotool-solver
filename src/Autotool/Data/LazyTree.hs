{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.Data.LazyTree
    ( Tree(Node)
    , Op
    , mkOp0
    , mkOp0C
    , mkOp1
    , mkOp1C
    , mkOp2
    , mkOp2C
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

data Op c a = Op0 String (c -> a)| Op1 String (c -> a -> a) | Op2 String Bool (c -> a -> a -> a)

mkOp0 :: String -> a -> Op c a
mkOp0 s = mkOp0C s . const

mkOp0C :: String -> (c -> a) -> Op c a
mkOp0C = Op0

mkOp1 :: String -> (a -> a) -> Op c a
mkOp1 s = mkOp1C s . const

mkOp1C :: String -> (c -> a -> a) -> Op c a
mkOp1C = Op1

mkOp2 :: String -> Bool -> (a -> a -> a) -> Op c a
mkOp2 s comm = mkOp2C s comm . const

mkOp2C :: String -> Bool -> (c -> a -> a -> a) -> Op c a
mkOp2C = Op2

isOp0 :: Op c a -> Bool
isOp0 Op0 {} = True
isOp0 _ = False

isOp1 :: Op c a -> Bool
isOp1 Op1 {} = True
isOp1 _ = False

isOp2 :: Op c a -> Bool
isOp2 Op2 {} = True
isOp2 _ = False

-- | Matches commutative 2-arity operators
isCommutative :: Op c a -> Bool
isCommutative (Op2 _ c _) = c
isCommutative _ = False

-- | Matches non-commutative 2-arity operators
isNonCommutative :: Op c a -> Bool
isNonCommutative (Op2 _ c _) = not c
isNonCommutative _ = False

-- | Evals an operator with the given arguments
eval :: c -> Op c a -> [a] -> a
eval c (Op0 _ f) _ = f c
eval c (Op1 _ f) [a] = f c a
eval c (Op2 _ _ f) [a,b] = f c a b

-- | Strict version of `eval`
eval' :: c -> Op c a -> [a] -> a
eval' !c (Op0 _ f) _ = f c
eval' !c (Op1 _ f) [!a] = let r = f c a in r
eval' !c (Op2 _ _ f) [!a,!b] = let r = f c a b in r

-- | Recursevely evaluates a operator tree
evalTree :: c -> Tree (Op c b)-> b
evalTree c = foldTree (eval c)

-- | Strict version of `evalTree`
evalTree' :: c -> Tree (Op c b) -> b
evalTree' c = foldTree (eval' c)

instance (Eq a) => Eq (Op c a) where
    (Op0 a _) == (Op0 b _) = a == b
    (Op1 a _) == (Op1 b _) = a == b
    (Op2 a _ _) == (Op2 b _ _) = a == b
    _ == _ = False

instance Show (Op c a) where
    show (Op0 s _) = s
    show (Op1 s _) = s
    show (Op2 s _ _) = s

-- | Find a tree that evaluates to a given value
findTree :: (Eq a)
         => [Op c a]                -- ^ the operations defining the tree type
         -> c   -- ^the evaluation context
         -> a                     -- ^ the target value to match against
         -> Tree (Op c a)           -- ^ a tree that evaluates to `a` under `f`
findTree c ops t = searchTree c ops (==t)

-- | Find a tree that satifies a given predicate when evaluated
searchTree :: (Eq a)
           => [Op c a]              -- ^ the operations defining the tree type
           -> c     -- ^ the evaluation context
           -> (a -> Bool)             -- ^ the predicate the trees evaluation must satisfy
           -> Tree (Op c a)           -- ^ a tree that evaluates to `a` under `f`
searchTree ops c p = head $ filter (p . evalTree' c) factory
    where
        factory = trees ops

-- | Find a tree that evaluates to a given value by creating at most `lim` trees
--
-- see `Autotool.Data.LazyTree.findTree`
findTreeLim :: (Eq a) =>
    Int                      -- ^ limit of trees to create
    -> [Op c a]                      -- ^ the operations defining the tree type
    -> c
    -> a                        -- ^ the target value to match against
    -> Maybe (Tree (Op c a))      -- ^ a tree that evaluates to `a` under `f`
findTreeLim lim ops c t = searchTreeLim lim ops c (==t)

-- | see `Autotool.Data.LazyTree.searchTree`
searchTreeLim :: (Eq a) =>
    Int                         -- ^ limit of trees to create
    -> [Op c a]                   -- ^ the operations defining the tree type
    -> c
    -> (a -> Bool)              -- ^ the target value to match against
    -> Maybe (Tree (Op c a))      -- ^ a tree that evaluates to `a` under `f`
searchTreeLim lim ops c p = find (p . evalTree' c) (take lim factory)
    where
        factory = trees ops

{- | Create an (infinite) list of all possible trees under a given set of operators and constants
    
    It works be cycling repeatedly through individual _runs_, where a run
    consists of multiple operator runs (one for each given non-const operator).

    An operator run consists of building _n_ trees w/ one operator as tree root where
    the number of trees to build in this run is determined at the start of the run.
 -}
trees :: forall a c. [Op c a]       -- ^ the given operators
      -> [Tree (Op c a)]          -- ^ the list of trees
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
        go :: [Op c a]            -- ^ operators - the next tree will be constructed with first op from this list
           -> [[Tree (Op c a)]]   -- ^ values - the next tree will be construction with the first [values] from this list
           -> Int               -- ^ the current tree index
           -> Int               -- ^ the index where the current run started at
           -> Int               -- ^ the index where the previous run started at
           -> [Tree (Op c a)]
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

depth :: Tree (Op c a) -> Int
depth = length . levels

size :: Tree (Op c a) -> Int
size (Node _ xs) = 1 + sum (map size xs)

showTree :: (Show a) => Tree (Op c a) -> String
showTree = foldTree f
    where
        f (Op0 s _) _ = s
        f (Op1 s _) [a] =  s ++ "(" ++ a ++ ")"
        f (Op2 s _ _) [a,b] = brOp a ++ " " ++ s ++ " " ++ brOp b
            where
                brOp [c] = [c]
                brOp c = "(" ++ c ++ ")"

showFnTree :: (Show a) => Tree (Op c a) -> String
showFnTree = foldTree f
    where
        f (Op0 s _) _ = s ++ "()"
        f (Op1 s _) [a] =  s ++ "(" ++ a ++ ")"
        f (Op2 s _ _) [a,b] = s ++ "(" ++ a ++ "," ++ b ++ ")"
            where
                brOp [c] = [c]
                brOp c = "(" ++ c ++ ")"