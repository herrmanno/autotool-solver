module Autotool.Solver.SetsSpec (spec) where

import Prelude hiding ((+), (-))

import Test.Hspec
import Autotool.Parser.NestedSet (parseIntSet)
import Autotool.Data.SetOp ( (&), (+), (-), pow )
import Autotool.Data.LazyTree ( Tree(Node), Op(Op0) )
import Autotool.Solver.Sets (solve)

spec = do
    describe "sets" $ do
        it "finds term w/ a target value from a set of sets and operations on them (1)" $
            let 
                a = Op0 "a" $ parseIntSet "{1, 2}"
                b = Op0 "b" $ parseIntSet "{{3}}"
                r = parseIntSet "{{}, {1, 2, {3}}, {1, {3}}, {2, {3}}, {{3}}}"
                ops = [(-), (+), (&), pow, a, b]
                result = Node (-) [Node pow [Node (+) [Node a [], Node b []] ], Node (-) [Node pow [Node a []], Node pow [Node b []]] ]
            in solve ops r `shouldBe` result
        it "finds term w/ a target value from a set of sets and operations on them (2)" $
            let
                a = Op0 "a" $ parseIntSet "{{3, {}}}"
                b = Op0 "b" $ parseIntSet "{3, {1, {}, {2}}}"
                r = parseIntSet "{{{3, {}}}}"
                ops = [(-), (+), (&), pow, a, b]
                result = Node (-) [ Node pow [Node a []], Node pow [Node b []] ]
            in solve ops r `shouldBe` result