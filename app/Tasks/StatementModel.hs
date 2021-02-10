module Tasks.StatementModel (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
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

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let stm = toValue (statement desc) :: Statement
        r = solve stm
    Result $ show (toDao r)
    where toDao = toValue :: Interpretation -> DAO.Map DAO.Identifier Bool

newtype StatementModelDescription = StatementModelDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)