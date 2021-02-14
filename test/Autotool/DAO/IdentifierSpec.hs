module Autotool.DAO.IdentifierSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import qualified Autotool.DAO.Identifier as DAO

spec = do
    describe "DAO.Identifier" $ do
        it "should parse single lower char" $
            let s = "a"
                r = DAO.mkId 'a'
            in read s `shouldBe` r
        it "should parse single upper char" $
            let s = "A"
                r = DAO.mkId 'A'
            in read s `shouldBe` r
        it "shouldn't parse non-alpha char (1)" $
            let s = "1"
            in evaluate (read s :: DAO.Identifier) `shouldThrow` anyException
        it "shouldn't parse non-alpha char (1)" $
            let s = "?"
            in evaluate (read s :: DAO.Identifier) `shouldThrow` anyException
        it "shouldn't parse multiple chars" $
            let s = "ab"
            in evaluate (read s :: DAO.Identifier) `shouldThrow` anyException
