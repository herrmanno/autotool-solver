module Autotool.DAO.MultiSetSpec (spec) where

import Test.Hspec
-- import qualified Data.MultiSet as MS
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.MultiSet as DAO
-- import Autotool.Readable

spec = do
    describe "DAO.MultiSet" $ do
        it "should parse empty Set" $
            let s = "{}"
                r = DAO.mkMSet []
            in (read s :: DAO.MultiSet DAO.Identifier) `shouldBe` r
        it "should parse empty Set w/ spaces" $
            let s = "{ }"
                r = DAO.mkMSet []
            in (read s :: DAO.MultiSet DAO.Identifier) `shouldBe` r
        it "should parse nonempty MultiSet" $
            let s = "{a:1,b:2}"
                r = DAO.mkMSet [(DAO.mkId 'a', 1), (DAO.mkId 'b', 2)]
            in (read s :: DAO.MultiSet DAO.Identifier) `shouldBe` r
        it "should parse nonempty MultiSet w/ spaces" $
            let s = "{ a : 1 , b : 2 }"
                r = DAO.mkMSet [(DAO.mkId 'a', 1), (DAO.mkId 'b', 2)]
            in (read s :: DAO.MultiSet DAO.Identifier) `shouldBe` r
        it "should parse nonempty MultiSet w/ repeated value" $
            let s = "{ a : 1 , a : 2 }"
                r = DAO.mkMSet [(DAO.mkId 'a', 2)]
            in (read s :: DAO.MultiSet DAO.Identifier) `shouldBe` r