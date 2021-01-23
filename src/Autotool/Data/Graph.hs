{-# LANGUAGE TupleSections #-}

module Autotool.Data.Graph (Graph, mkGraph, mkI, mkK, mkP, mkC, kante, neighbours, complement, join, add, subgraph, isCircle, Color(..)) where

import Data.Set (fromList, Set, toList, (\\), notMember)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

-- FIXME: a graph may need a Name for output!
type Graph a = (Set a, Set (a,a))

-- constructors

mkGraph :: (Ord a) => [a] -> [(a,a)] -> Graph a
mkGraph vs es = (fromList vs, fromList es)

kante :: (Ord a) => a -> a -> (a,a)
kante = (,)

mkI :: Int -> Graph Int
mkI n = mkGraph [1..n] []

mkK :: Int -> Graph Int
mkK n = let vs = [1..n] in mkGraph vs (allEdges vs)

mkP :: Int -> Graph Int
mkP n = let vs = [1..n] in mkGraph vs (zip vs $ drop 1 vs)

mkC :: Int -> Graph Int
mkC n = let vs = [1..n] in mkGraph vs ((n,1):zip vs (drop 1 vs))

-- operations

neighbours :: (Eq a) => Graph a -> a -> [a]
neighbours (vs, es) v = mapMaybe f (toList es) where
    f (a,b)
        | a == v = Just b
        | b == v = Just a
        | otherwise = Nothing

complement :: (Ord a) => Graph a -> Graph a
complement (vs, es) = let esAll = fromList $ allEdges (toList vs) in (vs, diff esAll es)
    where diff ea eb = S.filter (\(a,b) -> (a,b) `notMember` eb && (b,a) `notMember` eb) ea

-- TODO: make join take a rename function
join :: (Ord a, Num a) => Graph a -> Graph a -> Graph a
join g@(vs, es) h@(hs,_) = (S.union vs vs', edges) where
    (vs', es') = rename renameF h
    edges = fromList $ concatMap (\v -> map (,v) (toList vs')) (toList vs)
    maxV = S.findMax vs
    minV' = S.findMin hs
    renameF = if maxV >= minV' then (+maxV) else id

add :: (Ord a, Num a) => Graph a -> Graph a -> Graph a
add (vs, es) h@(hs,_) = (S.union vs vs', S.union es es') where
    (vs', es') = rename renameF h
    maxV = S.findMax vs
    minV' = S.findMin hs
    renameF = if maxV >= minV' then (+maxV) else id

subgraph :: (Ord a) => Graph a -> [a] -> Graph a
subgraph (vs, es) vs' = (fromList vs', es') where
    es' = S.filter (\(a,b) -> a `elem` vs' && b `elem` vs') es

isCircle :: (Eq a) => Graph a -> Bool
isCircle g@(vs, es) = length vs == length es && all ((==2) . degree g) (toList vs)

degree :: (Eq a) => Graph a -> a -> Int
degree (_, es) v = length $ S.filter (\(a,b) -> a == v || b == v) es

-- utility functions

allEdges :: [a] -> [(a,a)]
allEdges [a,b] = [(a,b)]
allEdges (v:vs) = allEdges vs ++ concatMap (allEdges . (:[v])) vs
allEdges _ = []

rename :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
rename f (vs, es) = (vs', es') where
    vs' = S.map f vs
    es' = S.map f' es
    f' (a,b) = (f a, f b)

-- Colors

data Color = A | B | C | D | E | F deriving (Eq, Ord, Enum, Show)