{-# LANGUAGE TupleSections #-}

module Autotool.Solver.ColorGraph (solve) where

import Data.Map (Map, (!), fromList, insert)
import Data.Set (toList)
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (notElem,permutations, permutations)
import Autotool.Data.Graph (Graph, mkGraph, kante, neighbours, Color(..))

allColorings :: (Ord a) => [a] -> [b] -> [Map a b]
allColorings [v] cs = [fromList $ map (v,) cs]
allColorings (v:vs) cs =
    let cms = allColorings vs cs
    in concatMap (\c -> map (insert v c) cms) cs

isValidColoring :: (Ord a, Eq b) => Graph a -> Map a b -> Bool
isValidColoring g@(vs, es) csm = all f vs where
    f v = let   c = csm ! v
                -- es' = S.map (\(a,b) -> if a == v then (b,a) else (a,b)) $ S.filter (\(a,b) -> a == v || b == v) es
                -- in all ((/=c) . (csm !) . snd) (toList es')
                in all ((/=c) . (csm !)) $ neighbours g v

solve :: (Ord a, Eq b) => Graph a -> [b] -> Map a b
solve g@(vs, _) cs = head $ filter (isValidColoring g) (allColorings (toList vs) cs)