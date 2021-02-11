module Tasks.Sets (task) where

import Prelude hiding ((+), (-))
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO ( DAO(toValue) )
import Autotool.DAO.NestedSet (NestedSet, SetOp(..))
import Autotool.DAO.Map (Map)
import Autotool.DAO.Identifier (mkId, Identifier)
import Autotool.Data.LazyTree (Op, mkOp0, showTree)
import Autotool.Data.NestedSet (NSet)
import Autotool.Solver.Sets (solve, solveP)

task :: Task
task = Task
    { runTask = run
    , name = "sets"
    , autotoolName = "Mengen"
    , description = "Finds an expression that evaluates to a given set"
    , longDescription = "Finds an expression that evaluates to a given set"
    , parameters =
        [ ("operators", "The operators the expression may contain")
        , ("sets", "The given sets the expression may contain")
        , ("target", "The value the expression should match.")
        ]
    , exampleInput = show $ SetDescription
        { operators = read "[+, &, -, pow]"
        , sets = read "[ (A, {{}, {{}}}), (B, {1, {1}, {2, {}}}) ]"
        , target = read "{{}, {{}, {{}}}, {{{}}}}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = toValue (operators desc) :: [Op () (NSet Int)]
        sops = map (\(name,set) -> mkOp0 (show name) (toValue set)) (sets desc) :: [Op () (NSet Int)]
        t = toValue (target desc) :: NSet Int
        r = solveP (sops ++ ops) t
    Result $ showTree r

data SetDescription = SetDescription
    { operators :: [SetOp Int]
    , sets :: [(Identifier, NestedSet Int)]
    , target :: NestedSet Int
    } deriving (Show,Read)