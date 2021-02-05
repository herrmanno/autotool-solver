module Tasks.StatementModel (runTask) where

import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import qualified Autotool.DAO.Map as DAO
import qualified Autotool.DAO.Identifier as DAO
import Autotool.Data.StatementLogic (Statement, Interpretation)
import Autotool.Solver.StatementModel (solve)


runTask :: String -> String
runTask s = show $ (toValue :: Interpretation -> DAO.Map DAO.Identifier Bool ) $ solve stm
    where
        desc = read s :: StatementModelDescription
        stm = toValue (statement desc) :: Statement

newtype StatementModelDescription = StatementModelDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)