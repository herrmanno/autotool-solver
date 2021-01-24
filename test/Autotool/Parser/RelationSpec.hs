module Autotool.Parser.RelationSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import Autotool.Parser.Relation (parseIntRelation)

spec = do
    describe "Relations" $ do
        it "should parse empty set" $
            let s = "{}"
                r = S.empty
            in parseIntRelation s `shouldBe` r
        it "should parse nonempty set (1)" $
            let s = "{(1,1)}"
                r = S.singleton (1,1)
            in parseIntRelation s `shouldBe` r
        it "should parse nonempty set (2)" $
            let s = "{(1,2),(2,3)}"
                r = S.fromList [(1,2),(2,3)]
            in parseIntRelation s `shouldBe` r
        it "should parse spaces in tuple" $
            let s = "{( 1 , 2 )}"
                r = S.fromList [(1,2)]
            in parseIntRelation s `shouldBe` r