module Autotool.Solver.HamiltonSpec (spec) where

import Test.Hspec
import Autotool.Data.Graph (mkGraph, kante)
import Autotool.Solver.Hamilton (solve, isHamiltonCircle)

spec = do
    describe "hamilton path" $ do
        it "finds a hamilton path in a graph (1)" $
             let g = mkGraph [1..10] [ kante 1 4
                       , kante 1 7
                       , kante 1 9
                       , kante 2 3
                       , kante 2 5
                       , kante 2 6
                       , kante 2 7
                       , kante 2 8
                       , kante 2 10
                       , kante 3 4
                       , kante 3 6
                       , kante 4 6
                       , kante 4 8
                       , kante 4 9
                       , kante 4 10
                       , kante 5 6
                       , kante 5 7
                       , kante 7 9
                       , kante 8 10
                       , kante 9 10
                       ] 
            in isHamiltonCircle g (solve g) `shouldBe` True
        it "finds a hamilton path in a graph (2)" $
             let g = mkGraph [1..12] [ kante 1 2
                       , kante 1 5
                       , kante 1 7
                       , kante 1 11
                       , kante 1 12
                       , kante 2 5
                       , kante 2 8
                       , kante 2 11
                       , kante 3 4
                       , kante 3 12
                       , kante 4 5
                       , kante 4 7
                       , kante 4 9
                       , kante 5 6
                       , kante 5 10
                       , kante 6 7
                       , kante 6 9
                       , kante 6 10
                       , kante 7 11
                       , kante 8 9
                       , kante 8 10
                       , kante 8 12
                       , kante 9 12
                       , kante 10 12
                       , kante 11 12
                       ]
            in isHamiltonCircle g (solve g) `shouldBe` True
        it "finds a hamilton path in a graph (3)" $
            let g = mkGraph [1..12] [ kante 1 2
                       , kante 1 4
                       , kante 1 6
                       , kante 1 8
                       , kante 1 10
                       , kante 2 3
                       , kante 2 5
                       , kante 2 7
                       , kante 2 8
                       , kante 2 9
                       , kante 3 6
                       , kante 3 11
                       , kante 3 12
                       , kante 4 10
                       , kante 5 7
                       , kante 5 8
                       , kante 5 9
                       , kante 5 12
                       , kante 6 9
                       , kante 7 11
                       , kante 8 11
                       , kante 10 12
                       ]
            in isHamiltonCircle g (solve g) `shouldBe` True
        it "finds a hamilton path in a graph (4)" $
            let g = mkGraph [1..12] [ kante 1 4
                       , kante 1 6
                       , kante 1 8
                       , kante 1 10
                       , kante 1 12
                       , kante 2 4
                       , kante 2 7
                       , kante 2 9
                       , kante 2 10
                       , kante 2 12
                       , kante 3 4
                       , kante 3 7
                       , kante 3 10
                       , kante 4 8
                       , kante 5 6
                       , kante 5 9
                       , kante 5 12
                       , kante 6 8
                       , kante 6 9
                       , kante 6 11
                       , kante 7 10
                       , kante 8 11
                       , kante 9 11
                       ] 
            in isHamiltonCircle g (solve g) `shouldBe` True

