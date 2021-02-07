module Autotool.Solver.StatementDNFSpec (spec) where

import Prelude hiding ((&&), (||))
import Data.Function (on)
import Test.Hspec ( describe, it, shouldBe )
import Autotool.Data.LazyTree (trees)
import Autotool.Data.StatementLogic (Statement(..), equiv, var, true, false, (&&), (||), (!), (-->), (<-->))
import Autotool.Solver.StatementDNF (solve)
import Debug.Trace (trace, traceShow)

spec =
    describe "DNF for statement" $ do
    it "finds arbritary DNFs that are semantically equivalent to the root statement" $
        let ops = [var 'a', var 'b', var 'c', true, false, (&&), (||), (!), (-->), (<-->)]
            ss = map Statement (take 10000 $ trees ops)
            cnfs = map solve ss
            allEquivalents = and $ zipWith equiv ss cnfs
        in allEquivalents `shouldBe` True