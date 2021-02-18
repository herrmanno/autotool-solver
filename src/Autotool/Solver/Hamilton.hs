module Autotool.Solver.Hamilton (solve, isHamiltonCircle) where

import Data.Set (delete, toList, member, size)
import qualified Data.Set as S
import Control.Monad (guard)
import Autotool.Data.Graph (Graph, neighbours, containsEdge)

solve :: (Eq a, Ord a, Show a) => Graph a -> [a]
solve g@(vs,alles) = take (size vs) $ go g [] where
    go :: (Eq a, Ord a, Show a) => Graph a -> [a] -> [a]
    go g@(vs, es) p
        | null vs = p
        | null p = let v0 = S.findMin vs in go (rm g v0) [v0]
        | otherwise = do
            let pn = head p
            v <- toList vs
            guard $ v `notElem` p
            guard $ containsEdge g (v, pn)
            guard $ size vs > 1 || containsEdge g (v, last p)
            go (rm g v) (v:p)
    rm (vs,es) v = (S.delete v vs, es)

isHamiltonCircle :: (Ord a) => Graph a -> [a] -> Bool
isHamiltonCircle g@(_, es) p = all (containsEdge g) p' where
    p' = (head p, last p) : zip p (drop 1 p)