module Autotool.DAO.SetSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import Autotool.DAO
import Autotool.Readable
import Autotool.DAO.Set

spec = do
    describe "DAO.Set" $ do
        it "should parse empty Set" $
            let s = "mkSet []"
                r = mkSet []
            in (read s :: Set Int) `shouldBe` r
        it "should parse empty Set w/ spaces" $
            let s = "mkSet [ ]"
                r = mkSet []
            in (read s :: Set Int) `shouldBe` r
        it "should parse empty BraceletSet" $
            let s = "{}"
                r = mkBrSet []
            in (read s :: Set Int) `shouldBe` r
        it "should parse empty Bracelet w/ spaces" $
            let s = "{ }"
                r = mkBrSet []
            in (read s :: Set Int) `shouldBe` r
        it "should parse nonempty Set" $
            let s = "mkSet [1,2]"
                r = mkSet [1,2]
            in (read s :: Set Int) `shouldBe` r
        it "should parse nonempty Set w/ spaces" $
            let s = "mkSet [ 1 , 2 ]"
                r = mkSet [1,2]
            in (read s :: Set Int) `shouldBe` r
        it "should parse nonempty BraceletSet" $
            let s = "{1,2}"
                r = mkBrSet [1,2]
            in (read s :: Set Int) `shouldBe` r
        it "should parse nonempty BraceletSet w/ spaces" $
            let s = "{ 1 , 2 }"
                r = mkBrSet [1,2]
            in (read s :: Set Int) `shouldBe` r
        it "should parse shown Set" $
            let str = "mkSet [ 1 , 2 ]"
                s = read str :: Set Int
                str' = show s
                r = S.fromList [1,2] :: S.Set Int
            in (toValue (read str' :: Set Int) :: S.Set Int) `shouldBe` r
        it "should parse shown BraceletSet" $
            let str = "{ 1 , 2 }"
                s = read str :: Set Int
                str' = show s
                r = S.fromList [1,2] :: S.Set Int
            in (toValue (read str' :: Set Int) :: S.Set Int) `shouldBe` r
        it "should extract internal Data.Set (1)" $
            let s = mkSet [1,2,3] :: Set Int
                r = S.fromList [1,2,3]
            in (toValue s :: S.Set Int) `shouldBe` r
        it "should extract internal Data.Set (2)" $
            let s = mkBrSet [1,2,3] :: Set Int
                r = S.fromList [1,2,3]
            in (toValue s :: S.Set Int) `shouldBe` r