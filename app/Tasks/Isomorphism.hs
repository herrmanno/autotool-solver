module Tasks.Isomorphism (task) where

import Task (Task(..), TaskInput, TaskResult(..), readInputM)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Graph as DAO ( Graph )
import Autotool.DAO.Map (mapToFM)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Isomorphism (solve)



task :: Task
task = Task
    { runTask = run
    , name = "iso"
    , autotoolName = "Graph-Iso"
    , description = "Finds an isomorphism from one graph to another"
    , longDescription = "Finds an isomorphism from one graph to another"
    , parameters =
        [ ("graph1", "The graph which is the source of the isomorphism")
        , ("graph2", "The graph which is the target of the isomorphism")
        ]
    , exampleInput = show $ IsomorphismDescription
        { graph1 = read "Graph { knoten = mkSet [ 0, 1, 2, 3] , kanten = mkSet [ kante 0 1 , kante 0 3 , kante 1 2 , kante 1 3 ] }"
        , graph2 = read "Graph { knoten = mkSet [ 0, 1, 2, 3] , kanten = mkSet [ kante 0 1 , kante 0 3 , kante 1 3 , kante 2 3 ] }"
        }
    }


run :: TaskInput -> TaskResult String
run s = do
    desc <- readInputM s
    let g = toValue (graph1 desc) :: G.Graph Int
        h = toValue (graph2 desc) :: G.Graph Int
        r = solve g h -- TODO: solve (findIsomorphism) may return Either
    case r of
        (Just iso) -> Result $ show $ mapToFM iso
        _ -> Error "ERROR: Cannot find an isomorphism from g to h"

data IsomorphismDescription = IsomorphismDescription
    { graph1 :: DAO.Graph Int
    , graph2 :: DAO.Graph Int
    } deriving (Show,Read)