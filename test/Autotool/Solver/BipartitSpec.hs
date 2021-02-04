module Autotool.Solver.BipartitSpec (spec) where

import Test.Hspec
import Autotool.Data.Graph (mkGraph, kante)
import Autotool.Solver.Bipartit (solve)

spec = do
    describe "bipartit" $ do
        it "finds a bipartit subgraph (1)" $
            let g = mkGraph [0..3]  [ kante 0 1
                       , kante 0 2
                       , kante 1 3
                       , kante 2 3
                       ] 
            in solve g `shouldBe` [0,3]
        it "finds a bipartit subgraph (2)" $
            let g = mkGraph [1..10] [ kante 1 3
                       , kante 1 9
                       , kante 1 10
                       , kante 2 3
                       , kante 2 5
                       , kante 2 8
                       , kante 2 9
                       , kante 2 10
                       , kante 3 4
                       , kante 3 7
                       , kante 4 8
                       , kante 4 9
                       , kante 4 10
                       , kante 5 6
                       , kante 5 7
                       , kante 6 9
                       , kante 7 8
                       , kante 7 10
                       ] 
            in solve g `shouldBe` [1,2,4,6,7]
