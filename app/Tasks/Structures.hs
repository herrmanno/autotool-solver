module Tasks.Structures (task) where


import Task (Task(..), TaskResult(..), readInputM, TaskInput)
import Autotool.Data.LazyTree (showFnTree)
import Autotool.DAO.Structure ( Struktur(..) )
import Autotool.Solver.Structures (solve)

task :: Task
task = Task
    { runTask = run
    , name = "structs"
    , autotoolName = "Term"
    , description = "Finds an expression with different value (semantically) in two structs"
    , longDescription = unlines 
        [ "Finds an expression with different value (semantically) in two structs"
        , "Note: Does currently only work on *two* structs. Also the structs need to have the"
        , "same signature."
        ]
    , parameters =
        [ ("structs", "The structs that shall evaluate the searched expression to different values")
        , ("limit", "The maxixum number of terms to try")
        ]
    , exampleInput = show $ StructureDescription
        { limit = 300000
        , structs = read "[ Struktur { universum = mkSet [ 1, 2, 3, 4] , predicates = listToFM    [ ] , functions = listToFM [ ( f , {(1 , 1 , 1) , (1 , 2 , 3) , (1 , 3 , 4) , (1 , 4 , 3) , (2 , 1 , 3) , (2 , 2 , 1) , (2 , 3 , 4) , (2 , 4 , 4) , (3 , 1 , 3) , (3 , 2 , 1) , (3 , 3 , 2) , (3 , 4 , 4) , (4 , 1 , 2) , (4 , 2 , 1) , (4 , 3 , 4) , (4 , 4 , 4)} ) , ( g , {(1 , 4) , (2 , 2) , (3 , 3) , (4 , 4)} ) , ( c, {(2)}) ] }, Struktur { universum = mkSet [ 1, 2, 3, 4] , predicates = listToFM    [ ] , functions = listToFM [ ( f , {(1 , 1 , 1) , (1 , 2 , 3) , (1 , 3 , 4) , (1 , 4 , 3) , (2 , 1 , 3) , (2 , 2 , 1) , (2 , 3 , 4) , (2 , 4 , 4) , (3 , 1 , 3) , (3 , 2 , 1) , (3 , 3 , 2) , (3 , 4 , 4) , (4 , 1 , 2) , (4 , 2 , 1) , (4 , 3 , 3) , (4 , 4 , 4)} ) , ( g , {(1 , 4) , (2 , 2) , (3 , 3) , (4 , 4)} ) , ( c, {(2)}) ] } ]"
        }
    }


run :: TaskInput -> TaskResult String
run input = showFnTree . (\desc -> solve (limit desc) (structs desc)) <$> readInputM input

data StructureDescription = StructureDescription
    { limit :: Int
    , structs :: [Struktur]
    } deriving (Show,Read)