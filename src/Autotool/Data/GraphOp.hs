module Autotool.Data.GraphOp ((+), (*), co) where

import Prelude hiding ((+), (*))

import Autotool.Data.Graph ( Graph, complement, join, add )
import Autotool.Data.LazyTree ( Op, mkOp1, mkOp2 )

(+) :: (Num a, Ord a) => Op c (Graph a)
(+) = mkOp2 "+" True add

(*) :: (Num a, Ord a) => Op c (Graph a)
(*) = mkOp2 "*" False join

co :: (Ord a) => Op c (Graph a)
co = mkOp1 "co" complement