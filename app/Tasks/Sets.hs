module Tasks.Sets (task) where

import Data.Bifunctor (Bifunctor(bimap))
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO ( DAO(toValue) )
import qualified Autotool.DAO.NestedSet as DAO
import qualified Autotool.DAO.Map as DAO
import qualified Autotool.DAO.Binding as DAO
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
        , sets = read "[ A = {{}, {{}}}, B = {1, {1}, {2, {}}} ]"
        , target = read "{{}, {{}, {{}}}, {{{}}}}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = toValue (operators desc) :: [Op () (NSet Int)]
        sops = map (uncurry mkOp0 . bimap show toValue . DAO.toPair) (sets desc)
        t = toValue (target desc) :: NSet Int
        r = solveP (sops ++ ops) t
    Result $ showTree r

data SetDescription = SetDescription
    { operators :: [DAO.SetOp Int]
    , sets :: [DAO.Binding (DAO.NestedSet Int)]
    , target :: DAO.NestedSet Int
    } deriving (Show,Read)