{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Autotool.Data.LazyTree (Op(..), trees) where

import Data.Tree ( foldTree, levels, Tree(Node) )

data Op a = Op0 a | Op1 String (a -> a) | Op2 String (a -> a -> a)

isOp0 (Op0 _) = True
isOp0 _ = False

isOp1 (Op1 _ _) = True
isOp1 _ = False

isOp2 (Op2 _ _) = True
isOp2 _ = False

instance (Eq a) => Eq (Op a) where
    (Op0 a) == (Op0 b) = a == b
    (Op1 a _) == (Op1 b _) = a == b
    (Op2 a _) == (Op2 b _) = a == b
    _ == _ = False

instance (Show a) => Show (Op a) where
    show (Op0 a) = show a
    show (Op1 s _) = s
    show (Op2 s _) = s

-- |Lazy-builds a list of trees from a given operator and takes a specific one from the list
-- 
-- example: build arithmetic trees from a given set of constants (1,2)
--
-- >>> trees [Op0 1, Op0 2, Op1, "-" negate, Op2 "+" (+)] 0
--
trees :: [Op a]         -- ^ the given operators
      -> Int            -- ^ the tree index to retrieve
      -> Tree (Op a)    -- ^ the n-th tree
trees ops = (fmap f [0..] !!) where
    -- constants
    ar0 = filter isOp0 ops
    ar1 = filter isOp1 ops
    ar2 = filter isOp2 ops
    -- functions
    f n
        | n < length ar0 = Node (ar0 !! n) []
        | otherwise = let op = operator n in Node op $ children op n
    children op n = let tl = termsL n; tl2 = tl^2 in case op of
        (Op1 _ _) -> [trees ops (n - tl)]
        (Op2 _ _) -> [trees ops (n `rem` tl2 `div` tl), trees ops (n `rem` tl2 `rem` tl)]
        _ -> []
    operator n =
        let tl = termsL n
            ops = concatMap (replicate tl) ar1 ++ concatMap (replicate (2^tl)) ar2
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

evalTree = foldTree f where
    f (Op0 a) _  = a
    f (Op1 "-" _) xs  = negate $ head xs
    f (Op2 "+" _) xs  = sum xs

main = do
    let ops = [Op0 1, Op0 2, Op1 "-" negate, Op2 "+" (+)] :: [Op Int]
    let ts = map (trees ops) [0..]
    let vs = map (\t -> (t, evalTree t)) ts
    print $ levels $ fst $ head $ dropWhile ((/= -4) . snd) vs
-- main = print $ (treesLevelCount [2, 3, 4]) 3
-- main = print $ zip [0..20] $ map (termsLength [2, 1, 1]) [0..20]