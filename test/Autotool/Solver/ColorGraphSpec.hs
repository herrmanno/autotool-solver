module Autotool.Solver.ColorGraphSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Autotool.Data.Graph (mkGraph, kante)
import Autotool.Solver.ColorGraph (solve)

spec = do
    describe "color graph" $ do
        it "finds a valid coloring of given colors for a tree (1)" $
             let g = mkGraph [1..4] [ kante 1 3
                       , kante 1 4
                       , kante 2 3
                       , kante 2 4
                       , kante 3 4
                       ]
            in solve g ['A'..'C'] `shouldBe` M.fromList [(1,'A'),(2,'A'),(3,'B'),(4,'C')]
        it "finds a valid coloring of given colors for a tree (2)" $
            let h = mkGraph [1..10] [ kante 1 3
                       , kante 1 4
                       , kante 1 5
                       , kante 1 7
                       , kante 1 9
                       , kante 2 4
                       , kante 2 5
                       , kante 2 6
                       , kante 2 7
                       , kante 2 9
                       , kante 3 4
                       , kante 3 5
                       , kante 3 8
                       , kante 3 9
                       , kante 4 7
                       , kante 5 7
                       , kante 6 9
                       , kante 7 8
                       , kante 7 9
                       , kante 7 10
                       , kante 8 10
                       ] 
            in solve h ['A'..'C'] `shouldBe` M.fromList [(1,'A'),(2,'A'),(3,'B'),(4,'C'),(5,'C'),(6,'B'),(7,'B'),(8,'A'),(9,'C'),(10,'C')]
