{-# LANGUAGE FlexibleContexts #-}

module Autotool.Solver.Structures (solve) where

import Data.List (nub, find)

import Autotool.Data.LazyTree (Tree, Op(..),evalTree,  trees )
import Autotool.DAO ( toValue )
import Autotool.DAO.Structure ( Struktur(..) )

solve :: Int -> [Struktur] -> Tree (Op () Int)
solve lim ss = let
    ops = toValue ss
    ts = map trees ops
    evalTrees n = map (evalTree () . (!!n)) ts
    idx = find ((1<) . (length . nub . evalTrees)) [0..lim]
    in case idx of
        (Just idx) -> head ts !! idx
        _ -> error "No matching expression found"