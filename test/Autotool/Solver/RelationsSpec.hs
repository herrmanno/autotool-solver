module Autotool.Solver.RelationsSpec (spec) where

import Prelude hiding ((+), (-), (*))
import Test.Hspec
import Data.Set (Set)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Set as DAO
import Autotool.Data.RelOp ( (&), (+), (-), (*) )
import Autotool.Data.LazyTree ( Tree(Node), mkOp0 )
import Autotool.TreeSearch (SearchMode(..))
import Autotool.Solver.Relations (solve)

spec = do
    describe "relations" $ do
        it "finds term w/ a target value from a set of relations and operations on them (1)" $
            let
                r = mkOp0 "r" $ readSet "{(1,1),(1,2),(2,1)}"
                s = mkOp0 "s" $ readSet "{(1,3),(2,1),(2,2)}"
                t = readSet "{(1,3),(2,3)}"
                ops = [(+), (&), (-), (*), r, s]
                result =
                    Node (-) [
                        Node (*) [
                            Node r [],
                            Node s []
                        ],
                        Node r []
                    ]
            in solve (Serial 10000) ops [1,2,3] t `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them (2)" $
            let
                r = mkOp0 "r" $ readSet "{(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}"
                s = mkOp0 "s" $ readSet "{(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)}"
                t = readSet "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
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
            in solve (Serial 250000) ops [1,2,3,4] t `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them in parallel (1)" $
            let
                r = mkOp0 "r" $ readSet "{(1,1),(1,2),(2,1)}"
                s = mkOp0 "s" $ readSet "{(1,3),(2,1),(2,2)}"
                t = readSet "{(1,3),(2,3)}"
                ops = [(&), (-), (*), (+), r, s]
                result =
                    Node (-) [
                        Node (*) [
                            Node r [],
                            Node s []
                        ],
                        Node r []
                    ]
            in solve (Parallel 10000) ops [1,2,3] t `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them in parallel (2)" $
            let
                r = mkOp0 "r" $ readSet "{(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}"
                s = mkOp0 "s" $ readSet "{(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)}"
                t = readSet "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
                ops = [(&), (-), (*), (+), r, s]
                result =
                    Node (*) [
                        Node s [],
                        Node (*) [
                            Node (+) [
                                Node r [],
                                Node s []
                            ],
                            Node r []
                        ]
                    ]
            in solve (Parallel 10000) ops [1,2,4]  t `shouldBe` result

readSet :: String -> Set(Int,Int)
readSet s = let dao = read s :: DAO.Set (Int,Int) in toValue dao