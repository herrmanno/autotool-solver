module Autotool.DAO.BindingSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import qualified Autotool.DAO.Binding as DAO
import qualified Autotool.DAO.Identifier as DAO

spec = do
    describe "DAO.Binding" $ do
        it "should parse number binding" $
            let s = "a = 1"
                r = DAO.fromPair (DAO.mkId 'a', 1::Int)
            in read s `shouldBe` r
        it "should parse list (number) binding" $
            let s = "a = [1,2]"
                r = DAO.fromPair (DAO.mkId 'a', [1,2]::[Int])
            in read s `shouldBe` r
        it "should parse with arbritary spaces" $
            let s = "  a  =  1  "
                r = DAO.fromPair (DAO.mkId 'a', 1::Int)
            in read s `shouldBe` r
        it "shouldn't parse missing value" $
            let s = "a ="
            in evaluate (read s :: DAO.Binding Int) `shouldThrow` anyException
