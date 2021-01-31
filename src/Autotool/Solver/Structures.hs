{-# LANGUAGE FlexibleContexts #-}

module Autotool.Solver.Structures (solve) where

import Data.List (nub, find, sortOn)

import qualified Data.Map as M
import Autotool.Data.LazyTree (isOp0,  Tree, Op(..), findTreeLim,evalTree,  trees )
import Autotool.DAO ( toValue )
import Autotool.DAO.Structure ( Struktur(..) )

solve :: [Struktur] -> Tree (Op Int)
solve ss = let
    lim = 300000
    ops = map toValue ss
    ts = map trees ops
    evalTrees n = map (evalTree . (!!n)) ts
    idx = find ((1<) . (length . nub . evalTrees)) [0..lim]
    in case idx of
        (Just idx) -> head ts !! idx
        _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"