module Tasks.Sets (task) where

import Prelude hiding ((+), (-))
import Task (Task(..))
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

-- TODO: read limit and parallel flag from description
run :: String -> String
run input = showTree $ solveP (sops ++ ops) t
    where
        desc = read input :: SetDescription
        ops = (map toValue $ operators desc) :: [Op () (NSet Int)]
        sops = map (\(name,set) -> mkOp0 (show name) (toValue set)) (sets desc) :: [Op () (NSet Int)]
        t = (toValue $ target desc) :: NSet Int

data SetDescription = SetDescription
    { operators :: [SetOp Int]
    , sets :: [(Identifier, NestedSet Int)]
    , target :: NestedSet Int
    } deriving (Show,Read)