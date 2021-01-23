module Autotool.Solver.CircleSpec (spec) where

import Test.Hspec
import Autotool.Data.Graph (mkGraph, kante)
import Autotool.Solver.Circle (solve)

spec = do
    describe "circle" $ do
        it "finds a circle of given length in a tree (1)" $
            let g = mkGraph [1..6] [ kante 1 2
                    , kante 1 4
                    , kante 1 5
                    , kante 1 6
                    , kante 2 3
                    , kante 2 4
                    , kante 2 5
                    , kante 3 6
                    , kante 4 6
                    , kante 5 6
                    ] 
            in solve 4 g `shouldBe` [[1,2,3,6],[2,3,4,6],[2,3,5,6],[2,4,5,6]]
        it "finds a circle of given length in a tree (2)" $
            let g = mkGraph [1..10] [ kante 1 5
                    , kante 1 9
                    , kante 2 5
                    , kante 2 6
                    , kante 2 8
                    , kante 2 9
                    , kante 3 5
                    , kante 3 7
                    , kante 3 9
                    , kante 4 5
                    , kante 4 6
                    , kante 4 7
                    , kante 4 9
                    , kante 5 7
                    , kante 5 8
                    , kante 6 7
                    , kante 6 9
                    , kante 8 9
                    , kante 8 10
                    , kante 9 10
                    ]
            in solve 5 g `shouldBe` [[1,5,6,7,9],[5,6,7,8,9]]
