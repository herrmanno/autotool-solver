module Autotool.DAO.GraphSpec (spec) where

import Test.Hspec
import Autotool.Readable
import qualified Autotool.DAO.Graph as DAO

spec = do
    describe "DAO.Graph" $ do
        it "should parse and show graph (1)" $
            let s = "Graph {knoten = mkSet [1,2,3], kanten = mkSet [kante 1 2,kante 2 3]}"
                g = read s :: DAO.Graph Int
            in show g `shouldBe` s
        it "should parse and show graph (2)" $
            let s = "Graph {knoten = {1,2,3}, kanten = {kante 1 2,kante 2 3}}"
                g = read s :: DAO.Graph Int
            in show g `shouldBe` s
        it "should parse and show graph (3)" $
            let s = "Graph {knoten = mkSet [], kanten = mkSet []}"
                g = read s :: DAO.Graph Int
            in show g `shouldBe` s