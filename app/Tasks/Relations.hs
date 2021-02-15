module Tasks.Relations (task) where

import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Set as DAO
import qualified Autotool.DAO.Relation as DAO
import qualified Autotool.DAO.Binding as DAO
import Autotool.Data.LazyTree (Op, mkOp0, showTree)
import Autotool.Solver.Relations (solve)
import Autotool.TreeSearch (SearchMode(..), evalModeDescription)

task :: Task
task = Task
    { runTask = run
    , name = "rels"
    , autotoolName = "Rel, Relation"
    , description = "Finds an expression that evaluates to a given relation"
    , longDescription = "Finds an expression that evaluates to a given relation"
    , parameters =
        [ ("operators", "The operators the expression may contain")
        , ("universe", "The universe of individuals. Needed to calcuate a relation's reflexive closure.")
        , ("sets", "The given relations the expression may contain")
        , ("target", "The value the expression should match.")
        , ("mode", evalModeDescription)
        ]
    , exampleInput = show $ RelationDescription
        { mode = Parallel 250000
        , operators = read "[+, &, -, ., inverse , transitive_cl , reflexive_cl]"
        , universe = [1,2,3,4]
        , relations = read "[ R = {(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}, S = {(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)} ]"
        , target = read "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = toValue (operators desc) :: [Op [Int] (S.Set (Int,Int))]
        rops = map (uncurry mkOp0 . bimap show toValue . DAO.toPair) (relations desc)
        u = universe desc
        t = toValue (target desc) :: S.Set (Int,Int)
        m = mode desc
        r = solve m (rops ++ ops) u t
    Result $ showTree r

data RelationDescription = RelationDescription
    { mode :: SearchMode
    , operators :: [DAO.RelOp Int]
    , universe :: [Int]
    , relations :: [DAO.Binding (DAO.Set (Int, Int))]
    , target :: DAO.Set (Int,Int)
    } deriving (Show,Read)