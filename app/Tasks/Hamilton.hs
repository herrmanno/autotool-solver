module Tasks.Hamilton (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO ( toValue )
import qualified Autotool.DAO.Graph as DAO ( Graph )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Hamilton (solve)



task :: Task
task = Task
    { runTask = run
    , name = "hamilton"
    , autotoolName = "Hamilton"
    , description = "Finds a hamilton path in a graph"
    , longDescription = "Finds a hamilton path in a graph"
    , parameters = [ ("graph", "The graph to find a circle in") ]
    , exampleInput = show $ HamiltonDescription
        { graph = read "Graph { knoten = mkSet [ 1, 2, 3, 4, 5] , kanten = mkSet [ kante 1 2 , kante 1 4 , kante 1 5 , kante 2 3 , kante 2 4 , kante 3 4 , kante 3 5 , kante 4 5 ] }"
        }
    }


run :: TaskInput -> TaskResult String
run s = do
    desc <- readInputM s
    let g = toValue (graph desc) :: G.Graph Int
        r = solve g
    Result $ show r

newtype HamiltonDescription = HamiltonDescription { graph :: DAO.Graph Int } deriving (Show,Read)