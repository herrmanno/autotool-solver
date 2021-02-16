module Autotool.Solver.SetsSpec (spec) where

import Prelude hiding ((+), (-))

import Test.Hspec
import Autotool.DAO (toValue)
import Autotool.DAO.NestedSet (NestedSet)
import Autotool.Data.NestedSet (NSet)
import Autotool.Data.SetOp ( (&), (+), (-), pow )
import Autotool.Data.LazyTree ( Tree(Node), mkOp0 )
import Autotool.TreeSearch (SearchMode(..))
import Autotool.Solver.Sets (solve)

spec = do
    describe "sets" $ do
        it "finds term w/ a target value from a set of sets and operations on them (1)" $
            let 
                a = mkOp0 "a" (toValue (read "{1, 2}" :: NestedSet Int) :: NSet Int)
                b = mkOp0 "b" (toValue (read "{{3}}" :: NestedSet Int) :: NSet Int)
                r = toValue (read "{{}, {1, 2, {3}}, {1, {3}}, {2, {3}}, {{3}}}" :: NestedSet Int) :: NSet Int
                ops = [(-), (+), (&), pow, a, b]
                result = Node (-) [Node pow [Node (+) [Node a [], Node b []] ], Node (-) [Node pow [Node a []], Node pow [Node b []]] ]
            in solve (Serial 300000) ops r `shouldBe` result
        it "finds term w/ a target value from a set of sets and operations on them (2)" $
            let
                a = mkOp0 "a" (toValue (read "{{3, {}}}" :: NestedSet Int) :: NSet Int)
                b = mkOp0 "b" (toValue (read "{3, {1, {}, {2}}}" :: NestedSet Int) :: NSet Int)
                r = toValue (read "{{{3, {}}}}" :: NestedSet Int) :: NSet Int
                ops = [(-), (+), (&), pow, a, b]
                result = Node (-) [ Node pow [Node a []], Node pow [Node b []] ]
            in solve (Serial 10000) ops r `shouldBe` result
        it "finds term w/ a target value from a set of sets and operations on them in parallel (1)" $
            let 
                a = mkOp0 "a" (toValue (read "{1, 2}" :: NestedSet Int) :: NSet Int)
                b = mkOp0 "b" (toValue (read "{{3}}" :: NestedSet Int) :: NSet Int)
                r = toValue (read "{{}, {1, 2, {3}}, {1, {3}}, {2, {3}}, {{3}}}" :: NestedSet Int) :: NSet Int
                ops = [pow, (-), (+), (&), a, b]
                result = Node (-) [Node pow [Node (+) [Node a [], Node b []] ], Node (-) [Node pow [Node a []], Node pow [Node b []]] ]
            in solve (Parallel 10000) ops r `shouldBe` result
        it "finds term w/ a target value from a set of sets and operations on them in parallel (2)" $
            let
                a = mkOp0 "a" (toValue (read "{{3, {}}}" :: NestedSet Int) :: NSet Int)
                b = mkOp0 "b" (toValue (read "{3, {1, {}, {2}}}" :: NestedSet Int) :: NSet Int)
                r = toValue (read "{{{3, {}}}}" :: NestedSet Int) :: NSet Int
                ops = [pow, (-), (+), (&), a, b]
                result = Node (-) [ Node pow [Node a []], Node pow [Node b []] ]
            in solve (Parallel 10000) ops r `shouldBe` result