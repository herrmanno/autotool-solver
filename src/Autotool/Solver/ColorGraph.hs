{-# LANGUAGE TupleSections #-}

module Autotool.Solver.ColorGraph (solve) where

import Data.Map (Map, (!), fromList, insert)
import Data.Set (toList)
import Autotool.Data.Graph (Graph, neighbours)

allColorings ::
            (Ord a) => [a]          -- ^ vertices
                    -> [b]          -- ^ colors
                    -> [Map a b]    -- ^ Colorings maps
allColorings [v] cs = [fromList $ map (v,) cs]
allColorings (v:vs) cs =
    let cms = allColorings vs cs
    in concatMap (\c -> map (insert v c) cms) cs

isValidColoring :: (Ord a, Eq b) => Graph a -> Map a b -> Bool
isValidColoring g@(vs, es) csm = all f vs where
    f v = let   c = csm ! v
                in all ((/=c) . (csm !)) $ neighbours g v

solve :: (Ord a, Eq b) => Graph a -> [b] -> Map a b
solve g@(vs, _) cs = head $ filter (isValidColoring g) (allColorings (toList vs) cs)