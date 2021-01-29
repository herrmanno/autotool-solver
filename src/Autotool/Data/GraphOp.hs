module Autotool.Data.GraphOp ((+), (*), co) where

import Prelude hiding ((+), (*))

import Autotool.Data.Graph ( Graph, complement, join, add )
import Autotool.Data.LazyTree ( Op(Op1, Op2) )

(+) :: (Num a, Ord a) => Op (Graph a)
(+) = Op2 "+" True add

(*) :: (Num a, Ord a) => Op (Graph a)
(*) = Op2 "*" False join

co :: (Ord a) => Op (Graph a)
co = Op1 "co" complement