module Autotool.Solver.SetsSpec (spec) where

import Test.Hspec
import Autotool.Data.Set
import Autotool.Data.Op
import Autotool.Data.Tree
import Autotool.Solver.Sets (solve)

spec = do
    describe "sets" $ do
        it "finds term w/ a target value from a set of sets and operations on them (1)" $
            let a = Set "A" [ V 1, V 2 ]
                b = Set "B" [ S [V 3] ]
                r = S[ S[], S[V 1, V 2, S [V 3]], S[V 1, S[V 3]], S[V 2, S[V 3]], S[S[V 3]] ]
                op2s = [Add, And, Subtr]
                op1s = [Pow]
                result = Node2 Subtr (Node1 Pow (Node2 Add (Node0 a) (Node0 b))) (Node2 Subtr (Node1 Pow (Node0 a)) (Node1 Pow (Node0 b)))
            in solve op2s op1s [a,b] r 3 `shouldBe` result
        it "finds term w/ a target value from a set of sets and operations on them (2)" $
            let a = Set "A" [ S[ V 3, S[] ] ]
                b = Set "B" [ V 3, S[ V 1, S[], S[ V 2 ] ] ]
                r = S[ S[ S[ V 3, S[] ] ] ]
                op2s = [Add, And, Subtr]
                op1s = [Pow]
                result = Node2 Subtr (Node1 Pow (Node0 a)) (Node1 Pow (Node0 b))
            in solve op2s op1s [a,b] r 3 `shouldBe` result