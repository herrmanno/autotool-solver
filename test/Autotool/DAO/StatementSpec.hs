module Autotool.DAO.StatementSpec (spec) where

import Prelude hiding ((&&),(||))
import Test.Hspec
import Autotool.DAO (toValue)
import Autotool.Readable
import Autotool.Data.StatementLogic (true, false, Statement(..), var, (||), (&&), (!), (-->), (<-->))
import qualified Autotool.DAO.Statement as DAO
import Data.Tree (Tree(Node))

spec = do
    describe "DAO.Statement" $ do
        it "should parse true" $
            let s = toValue (read "true" :: DAO.Statement)
                r = Statement $ Node true []
            in s `shouldBe` r
        it "should parse false" $
            let s = toValue (read "false" :: DAO.Statement)
                r = Statement $ Node false []
            in s `shouldBe` r
        it "should parse variable" $
            let s = toValue (read "a" :: DAO.Statement)
                r = Statement $ Node (var 'a') []
            in s `shouldBe` r
        it "should parse negated variable" $
            let s = toValue (read "! a" :: DAO.Statement)
                r = Statement $ Node (!) [Node (var 'a') []]
            in s `shouldBe` r
        it "should parse disjunction" $
            let s = toValue (read "a || b" :: DAO.Statement)
                r = Statement $ Node (||) [Node (var 'a') [], Node (var 'b') []]
            in s `shouldBe` r
        it "should parse conjunction" $
            let s = toValue (read "a && b" :: DAO.Statement)
                r = Statement $ Node (&&) [Node (var 'a') [], Node (var 'b') []]
            in s `shouldBe` r
        it "should parse implication" $
            let s = toValue (read "a -> b" :: DAO.Statement)
                r = Statement $ Node (-->) [Node (var 'a') [], Node (var 'b') []]
            in s `shouldBe` r
        it "should parse equivalence" $
            let s = toValue (read "a <-> b" :: DAO.Statement)
                r = Statement $ Node (<-->) [Node (var 'a') [], Node (var 'b') []]
            in s `shouldBe` r
        it "should parse nested operations in correct precedence" $
            let s = toValue (read "!a -> true <-> c || d && e" :: DAO.Statement)
                r = Statement $
                    Node (<-->) [
                        Node (-->) [
                            Node (!) [ Node (var 'a') [] ],
                            Node true []
                        ],
                        Node (||) [
                            Node (var 'c') [],
                            Node (&&) [
                                Node (var 'd') [],
                                Node (var 'e') []
                            ]
                        ]
                    ]
            in s `shouldBe` r
