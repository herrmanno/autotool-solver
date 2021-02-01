module Autotool.DAO.NestedSetSpec (spec) where

import Test.Hspec ( describe, it, shouldBe )
import qualified Data.Set as S
import Autotool.DAO (toValue)
import Autotool.DAO.NestedSet ( NestedSet )
import Autotool.Data.NestedSet (NSet, ø, (+.), (&.))

spec = do
    describe "NestedSet" $ do
        it "should parse empty set" $
            let s = "{}"
            in show (read s :: NestedSet Int) `shouldBe` "{}"
        it "should parse empty set w/ spaces" $
            let s = " { } "
            in show (read s :: NestedSet Int) `shouldBe` "{}"
        it "should parse nonempty set (1)" $
            let s = "{1}"
            in show (read s :: NestedSet Int) `shouldBe` "{1}"
        it "should parse nonempty set (2)" $
            let s = "{1,2}"
            in show (read s :: NestedSet Int) `shouldBe` "{1, 2}"
        it "should parse nonempty set w/ spaces bewteen values (1)" $
            let s = "{1, 2}"
            in show (read s :: NestedSet Int) `shouldBe` "{1, 2}"
        it "should parse nonempty set w/ spaces bewteen values (2)" $
            let s = "{ 1 , 2 }"
            in show (read s :: NestedSet Int) `shouldBe` "{1, 2}"
        it "should parse nested empty set (1)" $
            let s = "{{}}"
            in show (read s :: NestedSet Int) `shouldBe` "{{}}"
        it "should parse nested empty set (2)" $
            let s = "{{},{{}}}"
            in show (read s :: NestedSet Int) `shouldBe` "{{}, {{}}}"
        it "should parse mixed set" $
            let s = "{1,{{}}}"
            in show (read s :: NestedSet Int) `shouldBe` "{1, {{}}}"
        it "should parse mixed set w/ spaces" $
            let s = "{ 1 , { { } } }"
            in show (read s :: NestedSet Int) `shouldBe` "{1, {{}}}"
        it "should convert to Data.NestedSet (1)" $
            let s = read "{}" :: NestedSet Int
                r = ø
            in (toValue s :: NSet Int) `shouldBe` r
        it "should convert to Data.NestedSet (2)" $
            let s = read "{1,2}" :: NestedSet Int
                r = 1 +. 2 +. ø
            in (toValue s :: NSet Int) `shouldBe` r
        it "should convert to Data.NestedSet (3)" $
            let s = read "{1,{}}" :: NestedSet Int
                r = 1 +. (ø &. ø)
            in (toValue s :: NSet Int) `shouldBe` r