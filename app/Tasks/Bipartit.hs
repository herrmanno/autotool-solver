module Tasks.Bipartit (task) where

import Task (Task(..))
import Autotool.DAO ( DAO(toValue) )
import qualified Autotool.DAO.Graph as DAO ( Graph )
import Autotool.DAO.Set (mkSet)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Bipartit (solve)

task :: Task
task = Task
    { runTask = run
    , name = "bipartit"
    , autotoolName = "Bipartit"
    , description = "Finds a set of vertices that split a graph into two bipartit subgraphs"
    , longDescription = "Finds a set of vertices that split a graph into two bipartit subgraphs"
    , parameters = [ ("graph", "The graph to split into two bipartit graphs") ]
    , exampleInput = show $ BipartitGraphsDescription
        { graph = read "Graph { knoten = mkSet [ 0, 1, 2, 3] , kanten = mkSet [ kante 0 1 , kante 0 2 , kante 1 3 , kante 2 3 ] }"
        }
    }

run :: String -> String
run s = show $ mkSet $ solve g
    where
        desc = read s :: BipartitGraphsDescription
        g = toValue (graph desc) :: G.Graph Int

newtype BipartitGraphsDescription = BipartitGraphsDescription
    { graph :: DAO.Graph Int
    } deriving (Show,Read)