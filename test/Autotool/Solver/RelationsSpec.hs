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
                r = Op0 "r" $ parseIntRelation "{(1,1),(1,2),(2,1)}"
                s = Op0 "s" $ parseIntRelation "{(1,3),(2,1),(2,2)}"
                t = parseIntRelation "{(1,3),(2,3)}"
                ops = [(+), (&), (-), (*), r, s]
                result =
                    Node (-) [
                        Node (*) [
                            Node r [],
                            Node s []
                        ],
                        Node r []
                    ]
            in solve ops t `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them (2)" $
            let
                r = Op0 "r" $ parseIntRelation "{(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}"
                s = Op0 "s" $ parseIntRelation "{(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)}"
                t = parseIntRelation "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
                ops = [(+), (&), (-), (*), r, s]
                result =
                    Node (+) [
                        Node (*) [
                            Node s [],
                            Node r []
                        ],
                        Node (*) [
                            Node s [],
                            Node (*) [
                                Node r [],
                                Node r []
                            ]
                        ]
                    ]
            in solve ops t `shouldBe` result