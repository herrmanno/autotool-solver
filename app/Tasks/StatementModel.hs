module Tasks.StatementModel (task) where

import Task (Task(..))
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import qualified Autotool.DAO.Map as DAO
import qualified Autotool.DAO.Identifier as DAO
import Autotool.Data.StatementLogic (Statement, Interpretation)
import Autotool.Solver.StatementModel (solve)


task :: Task
task = Task
    { runTask = run
    , name = "al-model"
    , autotoolName = "AL-Modell"
    , description = "Finds an interpretation that satisfies a given statement"
    , longDescription = "Finds an interpretation that satisfies a given statement"
    , parameters =
        [ ("statement", "The statement (formula) to find an interpretation for")
        ]
    , exampleInput = show $ StatementModelDescription
        { statement = read "(  x || ! y ||   z) && (! x ||   y ||   z)"
        }
    }

run :: String -> String
run s = show $ (toValue :: Interpretation -> DAO.Map DAO.Identifier Bool ) $ solve stm
    where
        desc = read s :: StatementModelDescription
        stm = toValue (statement desc) :: Statement

newtype StatementModelDescription = StatementModelDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)