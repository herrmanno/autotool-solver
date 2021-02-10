module Tasks.Relations (task) where

import qualified Data.Set as S
import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO (toValue)
import Autotool.DAO.Set (Set)
import Autotool.DAO.Relation (RelOp)
import Autotool.DAO.Identifier (Identifier)
import Autotool.Data.LazyTree (Op, mkOp0, showTree)
import Autotool.Solver.Relations (solveP)

task :: Task
task = Task
    { runTask = run
    , name = "rels"
    , autotoolName = "Rel, Relation"
    , description = "Finds an expression that evaluates to a given relation"
    , longDescription = "Finds an expression that evaluates to a given relation"
    , parameters =
        [ ("operators", "The operators the expression may contain")
        , ("sets", "The given relations the expression may contain")
        , ("target", "The value the expression should match.")
        ]
    , exampleInput = show $ RelationDescription
        { operators = read "[+, &, -, .]"
        , relations = read "[ (R, {(1 , 4) , (2 , 4) , (3 , 2) , (4 , 1)}), (S, {(1 , 4) , (2 , 2) , (2 , 3) , (4 , 4)}) ]"
        , target = read "{(1 , 1) , (1 , 4) , (2 , 1) , (2 , 2) , (2 , 4) , (4 , 1) , (4 , 4)}"
        }
    }

run :: TaskInput -> TaskResult String
run input = do
    desc <- readInputM input
    let ops = (map toValue $ operators desc) :: [Op () (S.Set (Int,Int))]
        rops = map (\(name,rel) -> mkOp0 (show name) (toValue rel)) (relations desc) :: [Op () (S.Set (Int,Int))]
        t = (toValue $ target desc) :: S.Set (Int,Int)
        r = solveP (rops ++ ops) t
    Result $ showTree r

data RelationDescription = RelationDescription
    { operators :: [RelOp Int]
    , relations :: [(Identifier, Set (Int, Int))]
    , target :: Set (Int,Int)
    } deriving (Show,Read)