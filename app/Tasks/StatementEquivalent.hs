module Tasks.StatementEquivalent (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), StatementOp)
import Autotool.Data.LazyTree ( showTree )
import Autotool.TreeSearch (SearchMode(..), evalModeDescription)
import Autotool.Solver.StatementEquivalent (solve)


task :: Task
task = Task
    { runTask = run
    , name = "al-equiv"
    , autotoolName = "AL-Umformen"
    , description = "Finds an equivalent statement by brute forcing"
    , longDescription = unlines
        [ "Finds an equivalent statement by brute forcing."
        , "Note: If the target statement is very large (e. g. more than 10"
        , "junctors / variables), or the source statement contains a lot operators"
        , "or variables this task may not be able to compute a result."
        , "If possible use the task 'al-trans' in this cases, which produces quite"
        , "bigger results, but does so in finite time."
        ]
    , parameters =
        [ ("statement", "The statement (formula) to find an equivalent form for")
        , ("operators", "The operators the target statement may contain")
        , ("mode", evalModeDescription)
        ]
    , exampleInput = show $ StatementEquivalentDescription
        { mode = Parallel 250000
        , statement = read "(p <-> q) -> r && q"
        , operators = read "[!, ||, &&]"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let stm = toValue (statement desc) :: Statement
        ops = toValue (operators desc) :: [StatementOp]
        m = mode desc
        r = solve m stm ops
    Result $ showTree (tree r)

data StatementEquivalentDescription = StatementEquivalentDescription
    { mode :: SearchMode
    , statement :: DAO.Statement
    , operators :: [DAO.StatementOp]
    } deriving (Show,Read)