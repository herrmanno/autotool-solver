module Autotool.Solver.GraphsSpec (spec) where

import Prelude hiding ((+), (*))

import Test.Hspec
import Autotool.Data.GraphOp ( (+), (*), co )
import Autotool.Data.Graph
import Autotool.Data.LazyTree (showTree,  Tree(Node), Op(Op1, Op0) )
import Autotool.Solver.Graphs (solve)

spec = do
    describe "graphs" $ do
        it "finds term w/ a target value from a set of graphs and operations on them (1)" $
            let
                t = mkGraph [0..15] [ kante 0 2
                       , kante 0 3
                       , kante 0 4
                       , kante 0 5
                       , kante 0 6
                       , kante 1 3
                       , kante 1 4
                       , kante 1 5
                       , kante 1 6
                       , kante 2 3
                       , kante 2 4
                       , kante 2 5
                       , kante 2 6
                       , kante 3 4
                       , kante 3 6
                       , kante 4 5
                       , kante 5 6
                       , kante 7 8
                       , kante 8 9
                       , kante 9 10
                       , kante 11 12
                       , kante 11 13
                       , kante 12 13
                       ]
                ks = map (\i -> Op0 ("K" ++ show i) (mkK i)) [1..5]
                ps = map (\i -> Op0 ("P" ++ show i) (mkP i)) [3..5]
                cs = map (\i -> Op0 ("C" ++ show i) (mkC i)) [3..5]
                ops = ks ++ ps ++ cs ++ [co, (+), (*)]
            in showTree (solve ops t) `shouldBe` "(co((P3) + (co(C4)))) + (((P4) + (K3)) + ((K1) + (K1)))"
        -- it "finds term w/ a target value from a set of graphs and operations on them (2)" $
        --     let
        --         t = mkGraph [0..19] [ kante 0 1
        --                , kante 0 2
        --                , kante 0 3
        --                , kante 0 5
        --                , kante 0 6
        --                , kante 1 2
        --                , kante 1 3
        --                , kante 1 5
        --                , kante 1 6
        --                , kante 2 3
        --                , kante 2 5
        --                , kante 2 6
        --                , kante 3 5
        --                , kante 3 6
        --                , kante 5 6
        --                , kante 7 8
        --                , kante 7 9
        --                , kante 7 10
        --                , kante 7 11
        --                , kante 8 9
        --                , kante 8 10
        --                , kante 8 11
        --                , kante 9 10
        --                , kante 9 11
        --                , kante 10 11
        --                , kante 14 18
        --                , kante 14 19
        --                , kante 15 18
        --                , kante 15 19
        --                , kante 16 18
        --                , kante 16 19
        --                , kante 17 18
        --                , kante 17 19
        --                ] 
        --         ks = map (\i -> Op0 ("K" ++ show i) (mkK i)) [1..5]
        --         ps = map (\i -> Op0 ("P" ++ show i) (mkP i)) [3..5]
        --         cs = map (\i -> Op0 ("C" ++ show i) (mkC i)) [3..5]
        --         ops = ks ++ ps ++ cs ++ [co, (+), (*)]
        --     in showTree (solve ops t) `shouldBe` ""