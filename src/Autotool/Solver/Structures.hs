{-# LANGUAGE FlexibleContexts #-}

module Autotool.Solver.Structures (solve) where

import Data.List (nub, find, sortOn)

import qualified Data.Map as M
import Autotool.Data.LazyTree (isOp0,  Tree, Op(..), findTreeLim,evalTree,  trees )
import Autotool.Data.Structure ( Struktur, functions, Identifier(Id), Function(..), maplet )

solve :: [Struktur] -> Tree (Op Int)
solve ss = let
    lim = 300000
    ops = map toOps ss
    ts = map trees ops
    evalTrees n = map (evalTree . (!!n)) ts
    idx = find ((1<) . (length . nub . evalTrees)) [0..lim]
    in case idx of
        (Just idx) -> head ts !! idx
        _ -> error $ "No matching tree found within first " ++ show lim ++ " candidates"

toOps :: Struktur -> [Op Int]
toOps s = ops
    where
        ops = map toOp $ M.toList $ functions s
        toOp (Id a, f@(Fn0 _)) = Op0 [a] (maplet f [])
        toOp (Id a, f@(Fn1 _)) = Op1 [a] (maplet f . (: []))
        toOp (Id a, f@(Fn2 _)) = Op2 [a] False (\ x y -> maplet f [x,y])