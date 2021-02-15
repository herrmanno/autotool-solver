module Autotool.Solver.GraphsSpec (spec) where

import Prelude hiding ((+), (*))

import Test.Hspec
import Autotool.Data.GraphOp ( (+), (*), co )
import Autotool.Data.Graph
import Autotool.Data.LazyTree (showTree,  Tree(Node), Op, mkOp0 )
import Autotool.TreeSearch (SearchMode(..))
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
                ks = map (\i -> mkOp0 ("K" ++ show i) (mkK i)) [1..5]
                ps = map (\i -> mkOp0 ("P" ++ show i) (mkP i)) [3..5]
                cs = map (\i -> mkOp0 ("C" ++ show i) (mkC i)) [3..5]
                ops = ks ++ ps ++ cs ++ [co, (+), (*)]
            -- in showTree (solve ops t) `shouldBe` "(co((P3) + (co(C4)))) + (((P4) + (K3)) + ((K1) + (K1)))"
            in showTree (solve (Parallel 10000) ops t) `shouldBe` "((co(P3)) * (C4)) + (((P4) + (K3)) + ((K1) + (K1)))"
        it "finds term w/ a target value from a set of graphs and operations on them (2)" $
            let
                t = mkGraph [0..10] [ kante 0 6
                    , kante 0 7
                    , kante 0 8
                    , kante 0 9
                    , kante 1 6
                    , kante 1 7
                    , kante 1 8
                    , kante 1 9
                    , kante 2 6
                    , kante 2 7
                    , kante 2 8
                    , kante 2 9
                    , kante 3 6
                    , kante 3 7
                    , kante 3 8
                    , kante 3 9
                    , kante 4 6
                    , kante 4 7
                    , kante 4 8
                    , kante 4 9
                    , kante 5 6
                    , kante 5 7
                    , kante 5 8
                    , kante 5 9
                    , kante 6 8
                    , kante 7 9
                    ]
                ks = map (\i -> mkOp0 ("K" ++ show i) (mkK i)) [1..5]
                ps = map (\i -> mkOp0 ("P" ++ show i) (mkP i)) [3..5]
                cs = map (\i -> mkOp0 ("C" ++ show i) (mkC i)) [3..5]
                ops = ks ++ ps ++ cs ++ [co, (+), (*)]
            in showTree (solve (Serial 200000) ops t) `shouldBe` "(co(((K1) * (K5)) + (C4))) + (K1)"