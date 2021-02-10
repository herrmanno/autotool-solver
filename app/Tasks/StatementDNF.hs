module Tasks.StatementDNF (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.Data.LazyTree (showTree)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), Interpretation)
import Autotool.Solver.StatementDNF (solve)


task :: Task
task = Task
    { runTask = run
    , name = "al-dnf"
    , autotoolName = "DNF"
    , description = "Finds a semantically equivalent dnf for a given statement"
    , longDescription = "Finds a semantically equivalent dnf for a given statement"
    , parameters =
        [ ("statement", "The statement (formula) to find a dnf for")
        ]
    , exampleInput = show $ StatementDNFDescription
        { statement = read "true  -> (((q -> q) -> s) -> p)"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let stm = toValue (statement desc) :: Statement
        r = solve stm
    Result $ showTree $ tree r

newtype StatementDNFDescription = StatementDNFDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)