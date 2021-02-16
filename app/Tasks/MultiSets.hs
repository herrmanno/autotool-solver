module Tasks.MultiSets (task) where

import Data.Bifunctor (bimap)
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO ( DAO(toValue) )
import qualified Autotool.DAO.MultiSet as DAO
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.Binding as DAO
import Autotool.Data.LazyTree (mkOp0, showTree)
import Autotool.Data.MultiSet (MultiSet)
import Autotool.Data.MultiSetOp (MultiSetOp)
import Autotool.Solver.MultiSets (solve)
import Autotool.TreeSearch (SearchMode(..), evalModeDescription)

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
        , ("mode", evalModeDescription)
        ]
    , exampleInput = show $ MultiSetDescription
        { mode = Parallel 250000
        , operators = read "[+, &, -]"
        , sets = read "[ A = {p:1, q:3}, B = {q:2, r:3}, C = {q:1, r:1} ]"
        , target = read "{q:1}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = toValue (operators desc) :: [MultiSetOp () Char]
        sops = map (uncurry mkOp0 . bimap show toValue . DAO.toPair) (sets desc)
        t = toValue (target desc) :: MultiSet Char
        m = mode desc
        r = solve m (sops ++ ops) t
    Result $ showTree r

data MultiSetDescription = MultiSetDescription
    { mode :: SearchMode
    , operators :: [DAO.MultiSetOp]
    , sets :: [DAO.Binding (DAO.MultiSet DAO.Identifier)]
    , target :: DAO.MultiSet DAO.Identifier
    } deriving (Show,Read)