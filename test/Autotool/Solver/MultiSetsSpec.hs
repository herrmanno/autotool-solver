module Autotool.Solver.MultiSetsSpec (spec) where

import Prelude hiding ((+), (-))

import Test.Hspec
import Autotool.DAO (toValue)
import qualified Autotool.DAO.MultiSet as DAO
import qualified Autotool.DAO.Identifier as DAO
import Autotool.Data.MultiSetOp (MultiSet, (&), (+), (-) )
import Autotool.Data.LazyTree ( Tree(Node), Op, mkOp0, showTree )
import Autotool.Solver.MultiSets (solve, solveP)

spec = do
    describe "multisets" $ do
        it "finds term w/ a target value from a set of multisets and operations on them (1)" $
            let 
                a = mkOp0 "A" (toValue (read "{p:1, q:3}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                b = mkOp0 "B" (toValue (read "{q:2, r:3}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                c = mkOp0 "C" (toValue (read "{q:1, r:1}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                r = (toValue (read "{q:1}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                ops = [(-), (+), (&), a, b, c]
                result = "A & C"
            in showTree (solveP ops r) `shouldBe` result
        it "finds term w/ a target value from a set of multisets and operations on them (2)" $
            let 
                a = mkOp0 "A" (toValue (read "{q:3, t:3, u:1}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                b = mkOp0 "B" (toValue (read "{p:5, r:3, s:5, t:3}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                r = (toValue (read "{p:15, q:3, r:9, s:15, t:9, u:1}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                ops = [(-), (+), (&), a, b]
                result = "((A + B) + (B + B)) - (A & B)"
            in showTree (solveP ops r) `shouldBe` result
        it "finds term w/ a target value from a set of multisets and operations on them (3)" $
            let 
                a = mkOp0 "A" (toValue (read "{p:5, q:3, r:2, t:2}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                b = mkOp0 "B" (toValue (read "{s:2, t:1, u:5}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                c = mkOp0 "C" (toValue (read "{p:5, q:2, r:4, u:3}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                r = (toValue (read "{s:8, t:3, u:17}" :: DAO.MultiSet DAO.Identifier) :: MultiSet Char)
                ops = [(-), (+), (&), a, b, c]
                result = "(B + B) + ((B - A) + (B - C))"
            in showTree (solveP ops r) `shouldBe` result
