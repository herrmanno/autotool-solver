module Autotool.Solver.StatementEquivalentSpec (spec) where

import Prelude hiding ((&&), (||))
import Test.Hspec
import qualified Data.Map as M
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic ( Statement(Statement), var, (&&), (||), (!))
import Autotool.Solver.StatementEquivalent (solve)
import Data.Tree (Tree(Node))

spec = do
    describe "equivalent statement" $ do
        it "finds an equivalent statement built upon given ops by brute forcing (1)" $
            let s = readStatement "(x -> z) <-> (y -> z)"
                ops = [(&&), (||), (!)]
                r = readStatement "((x || z) || !y) && ((z || y) || !x)"
            in solve s ops `shouldBe` r
        it "finds an equivalent statement built upon given ops by brute forcing (2)" $
            let s = readStatement "y -> x"
                ops = [(!), (&&), (||)]
                r = readStatement "! y || (y && x)"
            in solve s ops `shouldBe` r

readStatement :: String -> Statement
readStatement = toValue . (read :: String -> DAO.Statement)

