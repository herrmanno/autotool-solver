module Tasks.StatementTransform (runTask) where

import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), StatementOp)
import Autotool.Solver.StatementTransform (solve)
import Autotool.Data.LazyTree ( showTree )


runTask :: String -> String
runTask s = showTree $ tree $ solve stm ops
    where
        desc = read s :: StatementTransformDescription
        stm = toValue (statement desc) :: Statement
        ops = map toValue (operators desc) :: [StatementOp]

data StatementTransformDescription = StatementTransformDescription
    { statement :: DAO.Statement
    , operators :: [DAO.StatementOp]
    } deriving (Show,Read)