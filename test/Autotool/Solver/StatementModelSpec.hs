module Autotool.Solver.StatementModelSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic
import Autotool.Solver.StatementModel (solve)

spec = do
    describe "statement model" $ do
        it "find a model for a propositional statement (1)" $
            let s = readStatement "(  x || ! y ||   z) && (! x ||   y ||   z)"
            in solve s `shouldBe` M.fromList [('x',False),('y',False),('z',False)]
        it "find a model for a propositional statement (2)" $
            let s = readStatement "(  p ||   r || ! s) && (  p ||   s || ! q) && (  q ||   s || ! p) && (  q || ! p || ! r) && (  q || ! r || ! s) && (  r ||   s || ! q)"
            in solve s `shouldBe` M.fromList [('p',False),('r',False),('q',False),('s',False)]

readStatement :: String -> Statement
readStatement = toValue . (read :: String -> DAO.Statement)