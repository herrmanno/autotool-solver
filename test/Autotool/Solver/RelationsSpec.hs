module Autotool.Solver.RelationsSpec (spec) where

import Test.Hspec
import Autotool.Data.Set
import Autotool.Data.Op
import Autotool.Data.Tree
import Autotool.Solver.Relations (solve)

spec = do
    describe "relations" $ do
        it "finds term w/ a target value from a set of relations and operations on them (1)" $
            let r = Set "R" [ V(1 , 1), V(1 , 2), V(2 , 1) ] :: S (Int,Int)
                s = Set "S" [ V(1 , 3), V(2 , 1), V(2 , 2) ]
                t = S[ V(1 , 2) , V(2 , 3) ]
                op2s = [Add, And, Subtr, Compose]
                op1s = []
                result = Node2 Subtr (Node2 Compose (Node0 r) (Node2 Subtr (Node0 s) (Node0 r))) (Node0 s)
            in solve op2s op1s [s,r] t 3 `shouldBe` result
        it "finds term w/ a target value from a set of relations and operations on them (2)" $
            let r = Set "R" [ V(1 , 4), V(2 , 4), V(3 , 2), V(4 , 1) ] :: S (Int,Int)
                s = Set "S" [ V(1 , 4), V(2 , 2), V(2 , 3), V(4 , 4) ]
                t = S[ V(1 , 1) , V(1 , 4) , V(2 , 1) , V(2 , 2) , V(2 , 4) , V(4 , 1) , V(4 , 4) ]
                op2s = [Add, And, Subtr, Compose]
                op1s = []
                result = Node2 Compose (Node0 s) (Node2 Compose (Node2 Add (Node0 r) (Node0 s)) (Node0 r))
            in solve op2s op1s [s,r] t 3 `shouldBe` result