module Autotool.Parser.NestedSetSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import Autotool.Data.NestedSet ( (&.), (+.), ø, NSet )
import Autotool.Parser.NestedSet ( parseIntSet )

spec = do
    describe "NestedSet" $ do
        it "should parse empty set" $
            let s = "{}"
                r = ø
            in parseIntSet s `shouldBe` r
        it "should parse empty set w/ spaces" $
            let s = " { } "
                r = ø
            in parseIntSet s `shouldBe` r
        it "should parse nonempty set (1)" $
            let s = "{1}"
                r = 1 +. ø
            in parseIntSet s `shouldBe` r
        it "should parse nonempty set (2)" $
            let s = "{1,2}"
                r = 1 +. 2 +. ø
            in parseIntSet s `shouldBe` r
        it "should parse nonempty set w/ spaces bewteen values (1)" $
            let s = "{1, 2}"
                r = 1 +. 2 +. ø
            in parseIntSet s `shouldBe` r
        it "should parse nonempty set w/ spaces bewteen values (2)" $
            let s = "{ 1 , 2 }"
                r = 1 +. 2 +. ø
            in parseIntSet s `shouldBe` r
        it "should parse nested empty set (1)" $
            let s = "{{}}"
                r = ø &. ø
            in parseIntSet s `shouldBe` r
        it "should parse nested empty set (2)" $
            let s = "{{},{{}}}"
                r = ø &. (ø &. ø) &. ø :: NSet Integer
            in parseIntSet s `shouldBe` r
        it "should parse mixed set" $
            let s = "{1,{{}}}"
                r = 1 +. (ø &. ø) &. ø :: NSet Integer
            in parseIntSet s `shouldBe` r
        it "should parse mixed set w/ spaces" $
            let s = "{ 1 , { { } } }"
                r = 1 +. (ø &. ø) &. ø :: NSet Integer
            in parseIntSet s `shouldBe` r