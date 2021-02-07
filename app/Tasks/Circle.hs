module Tasks.Circle (task) where

import Task (Task(..))
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Graph as DAO ( Graph )
import Autotool.DAO.Set (mkSet)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Circle (solve)

task :: Task
task = Task
    { runTask = run
    , name = "circle"
    , autotoolName = "Kreis, Graph-Kreis"
    , description = "Finds a circle of given length in a graph"
    , longDescription = "Finds a circle of given length in a graph"
    , parameters = [ ("graph", "The graph to find a circle in"), ("length", "The lenght of the circle to find") ]
    , exampleInput = show $ CircleDescription
        { graph = read "Graph { knoten = mkSet [ 1, 2, 3, 4, 5, 6] , kanten = mkSet [ kante 1 2 , kante 1 4 , kante 1 5 , kante 1 6 , kante 2 3 , kante 2 4 , kante 2 5 , kante 3 6 , kante 4 6 , kante 5 6 ] }"
        , Tasks.Circle.length = 4
        }
    }

run :: String -> String
run s = unlines $ map (show . mkSet) $ solve l g
    where
        desc = read s :: CircleDescription
        l = Tasks.Circle.length desc
        g = toValue (graph desc) :: G.Graph Int

data CircleDescription = CircleDescription
    { graph :: DAO.Graph Int
    , length :: Int
    } deriving (Show,Read)