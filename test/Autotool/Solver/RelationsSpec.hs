module Autotool.Solver.RelationsSpec (spec) where

import Prelude hiding ((+), (-), (*))

import Test.Hspec
import Autotool.Data.RelOp ( (&), (+), (-), (*) )
import Autotool.Data.LazyTree ( Tree(Node), Op(Op0) )
import Autotool.Solver.Relations (solve)
import Autotool.Parser.Relation (parseIntRelation)

spec = do
    describe "relations" $ do
        it "finds term w/ a target value from a set of relations and operations on them (1)" $
            let
                r = parseIntRelation "{(1,1),(1,2),(2,1)}"
                s = parseIntRelation "{(1,3),(2,1),(2,2)}"
                t = parseIntRelation "{(1,3),(2,3)}"
                ops = [(+), (&), (-), (*), Op0 r, Op0 s]
                result =
                    Node (-) [
                        Node (*) [
                            Node (Op0 r) [],
                            Node (Op0 s) []
                        ],
                        Node (Op0 r) []
                    ]
            in solve ops t `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them (2)" $
            let
                r = parseIntRelation "{(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}"
                s = parseIntRelation "{(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)}"
                t = parseIntRelation "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
                ops = [(+), (&), (-), (*), Op0 r, Op0 s]
                result =
                    Node (*) [
                        Node (Op0 s) [],
                        Node (*) [
                            Node (+) [
                                Node (Op0 s) [],
                                Node (Op0 r) []
                            ],
                            Node (Op0 r) []
                        ]
                    ]
            in solve ops t `shouldBe` result