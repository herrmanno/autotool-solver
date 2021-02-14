module Tasks.MultiSets (task) where

import Prelude hiding ((+), (-))
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO ( DAO(toValue) )
import qualified Autotool.DAO.MultiSet as DAO
import qualified Autotool.DAO.Identifier as DAO
import Autotool.Data.LazyTree (Op, mkOp0, showTree)
import Autotool.Data.MultiSetOp (MultiSet, MultiSetOp)
import Autotool.Solver.MultiSets (solve, solveP)

task :: Task
task = Task
    { runTask = run
    , name = "multisets"
    , autotoolName = "MultiMengen, MM"
    , description = "Finds an expression that evaluates to a given multi set (bag)"
    , longDescription = "Finds an expression that evaluates to a given multi set (bag)"
    , parameters =
        [ ("operators", "The operators the expression may contain")
        , ("sets", "The given sets the expression may contain")
        , ("target", "The value the expression should match.")
        ]
    , exampleInput = show $ MultiSetDescription
        { operators = read "[+, &, -]"
        , sets = read "[ (A, {p:1, q:3}), (B, {q:2, r:3}), (C, {q:1, r:1}) ]"
        , target = read "{q:1}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = toValue (operators desc) :: [MultiSetOp () Char]
        sops = map (\(name,set) -> mkOp0 (show name) (toValue set)) (sets desc) :: [MultiSetOp () Char]
        t = toValue (target desc) :: MultiSet Char
        r = solveP (sops ++ ops) t
    Result $ showTree r

data MultiSetDescription = MultiSetDescription
    { operators :: [DAO.MultiSetOp]
    , sets :: [(DAO.Identifier, DAO.MultiSet DAO.Identifier)]
    , target :: DAO.MultiSet DAO.Identifier
    } deriving (Show,Read)