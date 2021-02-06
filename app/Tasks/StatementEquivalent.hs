module Tasks.StatementEquivalent (runTask) where

import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), StatementOp)
import Autotool.Solver.StatementEquivalent (solve)
import Autotool.Data.LazyTree ( showTree )


runTask :: String -> String
runTask s = showTree $ tree $ solve stm ops
    where
        desc = read s :: StatementEquivalentDescription
        stm = toValue (statement desc) :: Statement
        ops = map toValue (operators desc) :: [StatementOp]

data StatementEquivalentDescription = StatementEquivalentDescription
    { statement :: DAO.Statement
    , operators :: [DAO.StatementOp]
    } deriving (Show,Read)