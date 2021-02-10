module Tasks.StatementCNF (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.Data.LazyTree (showTree)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), Interpretation)
import Autotool.Solver.StatementCNF (solve)


task :: Task
task = Task
    { runTask = run
    , name = "al-cnf"
    , autotoolName = "CNF"
    , description = "Finds a semantically equivalent cnf for a given statement"
    , longDescription = "Finds a semantically equivalent cnf for a given statement"
    , parameters =
        [ ("statement", "The statement (formula) to find a cnf for")
        ]
    , exampleInput = show $ StatementCNFDescription
        { statement = read "true  -> (((q -> q) -> s) -> p)"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let stm = toValue (statement desc) :: Statement
        r = solve stm
    Result $ showTree $ tree r

newtype StatementCNFDescription = StatementCNFDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)