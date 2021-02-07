module Tasks.StatementTransform (task) where

import Task (Task(..))
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), StatementOp)
import Autotool.Solver.StatementTransform (solve)
import Autotool.Data.LazyTree ( showTree )


task :: Task
task = Task
    { runTask = run
    , name = "al-trans"
    , autotoolName = "AL-Umformen"
    , description = "Finds an equivalent statement by recursive transformation"
    , longDescription = "Finds an equivalent statement by recursive transformation."
    , parameters =
        [ ("statement", "The statement (formula) to find an equivalent form for")
        , ("operators", "The operators the target statement may contain")
        ]
    , exampleInput = show $ StatementTransformDescription
        { statement = read "(p <-> q) -> r && q"
        , operators = read "[!, ||, &&]"
        }
    }

run :: String -> String
run s = showTree $ tree $ solve stm ops
    where
        desc = read s :: StatementTransformDescription
        stm = toValue (statement desc) :: Statement
        ops = map toValue (operators desc) :: [StatementOp]

data StatementTransformDescription = StatementTransformDescription
    { statement :: DAO.Statement
    , operators :: [DAO.StatementOp]
    } deriving (Show,Read)