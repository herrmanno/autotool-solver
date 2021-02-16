module Autotool.Solver.Hamilton (solve) where

import Data.Set (delete, toList, member, size)
import Control.Monad (guard)
import Autotool.Data.Graph (Graph)

solve :: (Eq a, Ord a, Show a) => Graph a -> [a]
solve g@(vs,_) = take (size vs) $ go g [] where
    go :: (Eq a, Ord a, Show a) => Graph a -> [a] -> [a]
    go g@(vs, es) p
        | null vs = p
        | otherwise = do
            let vs' = toList vs
            v <- vs'
            guard $ v `notElem` p
            guard $
                null p ||
                (size vs == 1 && let x = last p in (v,x) `member` es || (x,v) `member` es) ||
                (size vs > 1 && let x = head p in (v,x) `member` es || (x,v) `member` es)
            let vs' = delete v vs
            let g' = (vs', es)
            let p' = v:p
            go g' p'

-- hamilton :: (Eq a, Ord a, Show a) => Graph a -> [a]
-- hamilton g@(vs, es) = let perms = permutations (toList vs) in head $ filter (isHamiltonCircle g) perms

isHamiltonCircle :: (Ord a) => Graph a -> [a] -> Bool
isHamiltonCircle (_, es) p = all f p' where
    p' = (head p, last p) : zip p (drop 1 p)
    f (a,b) = (a,b) `member` es || (b,a) `member` es
