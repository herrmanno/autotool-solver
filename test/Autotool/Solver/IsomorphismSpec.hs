module Autotool.Solver.IsomorphismSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Autotool.Data.Graph (mkGraph, kante)
import Autotool.Solver.Isomorphism (solve)

spec = do
    describe "isomorphism" $ do
        it "finds an isomorphism for two graphs (1)" $
            let g = mkGraph [0..3]  [ kante 0 1
                       , kante 0 3
                       , kante 1 2
                       , kante 1 3
                       ]
                h = mkGraph [0..3] [ kante 0 1
                       , kante 0 3
                       , kante 1 3
                       , kante 2 3
                       ]
            in solve g h `shouldBe` Just (M.fromList [ (0,1), (1,3), (2,2), (3,0) ])
        it "finds an isomorphism for two graphs (2)" $
            let g = mkGraph [1..7] [ kante 1 3
                       , kante 1 5
                       , kante 1 6
                       , kante 1 7
                       , kante 2 4
                       , kante 2 5
                       , kante 2 6
                       , kante 2 7
                       , kante 3 5
                       , kante 3 6
                       , kante 3 7
                       , kante 4 5
                       , kante 4 6
                       , kante 4 7
                       ]
                h = mkGraph [1..7] [ kante 1 4
                       , kante 1 5
                       , kante 1 6
                       , kante 1 7
                       , kante 2 3
                       , kante 2 4
                       , kante 2 6
                       , kante 2 7
                       , kante 3 4
                       , kante 3 6
                       , kante 3 7
                       , kante 4 5
                       , kante 5 6
                       , kante 5 7
                       ] 
            in solve g h `shouldBe` Just (M.fromList [(1,1),(2,2),(3,5),(4,3),(5,4),(6,6),(7,7)])
