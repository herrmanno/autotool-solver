module Tasks.Graphs (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.Data.LazyTree (showTree, Op)
import Autotool.TreeSearch (SearchMode(..),evalModeDescription)
import Autotool.DAO ( toValue )
import qualified Autotool.DAO.Graph as DAO ( Graph, GraphConst, GraphOp )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Graphs (solve)

task :: Task
task = Task
    { runTask = run
    , name = "graphs"
    , autotoolName = "Graph-Op"
    , description = "Finds an expression that evaluates to given graph"
    , longDescription = "Finds an expression that evaluates to given graph"
    , parameters =
        [ ("target", "The graph the searched expression should evaluate to")
        , ("graphs", "The given graphs the expression may contain")
        , ("operators", "The operators the expression may contain")
        , ("mode", evalModeDescription)
        ]
    , exampleInput = show $ GraphsDescription
        { mode = Parallel 250000
        , target = read "Graph { knoten = mkSet [ 0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 ] , kanten = mkSet [ kante 0 6 , kante 0 7 , kante 0 8 , kante 0 9 , kante 1 6 , kante 1 7 , kante 1 8 , kante 1 9 , kante 2 6 , kante 2 7 , kante 2 8 , kante 2 9 , kante 3 6 , kante 3 7 , kante 3 8 , kante 3 9 , kante 4 6 , kante 4 7 , kante 4 8 , kante 4 9 , kante 5 6 , kante 5 7 , kante 5 8 , kante 5 9 , kante 6 8 , kante 7 9 ] }"
        , graphs = read "[ K1 , K2 , K3 , K4 , K5 , P3 , P4 , P5 , C3 , C4 , C5 ] "
        , ops = read "[ *, +, co]"
        }
    }

run :: TaskInput -> TaskResult String
run s = do
    desc <- readInputM s
    let t = toValue $ target desc :: G.Graph Int
        operators = toValue $ ops desc :: [Op () (G.Graph Int)]
        consts = toValue $ graphs desc :: [Op () (G.Graph Int)]
        m = mode desc
        r = solve m (consts ++ operators) t
    Result $ showTree r

data GraphsDescription = GraphsDescription
    { mode :: SearchMode
    , target :: DAO.Graph Int
    , graphs :: [DAO.GraphConst]
    , ops :: [DAO.GraphOp Int]
    } deriving (Show,Read)