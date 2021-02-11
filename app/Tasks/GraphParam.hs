module Tasks.GraphParam (task) where


import Task (Task(..), TaskInput, TaskResult(..), readInputM, fromEither)
import Autotool.DAO ( DAO(toValue) )
import Autotool.DAO.Graph as DAO ( GraphConstraint, Graph )
import Autotool.DAO.Identifier as DAO (Identifier)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.GraphParam (solve)

task :: Task
task = Task
    { runTask = run
    , name = "graphparam"
    , autotoolName = "Graph-Param"
    , description = "Creates a graph from a given set of contraints"
    , longDescription = unlines
        [ "Creates a graph from a given set of contraints."
        , "Notice that the universe must match the vertices of the contraints"
        , "and its length must be the same as the 'vertices' constraint value, if given."
        ]
    , parameters =
        [ ("universe", "The universe of the graph. List all vertices here.")
        , ("constraints", "The contraints the graph must satisfy. Copy from Autotool here.")
        ]
    , exampleInput = show $ GraphParamDescription
        { universe = read "[a,b,c,d,e,f]"
        , constraints = read "[ vertices = 6 , edges = 10 , maxdegree = 4 , maxclique = 4 , edge ( f, a) , edge ( e, a) , edge ( e, f) , Not (edge ( b, c)) , Not (edge ( d, e)) , degree (b) = 3 , degree (c) = 3 , degree (e) = 4 , degree (d) = 2 , degree (f) = 4 ]"
        }
    }

run :: TaskInput -> TaskResult String
run s = do
    desc <- readInputM s
    let vs = toValue (universe desc) :: [Char]
        cs = toValue (constraints desc) :: [G.GraphConstraint Char]
        r = solve cs vs
    case r of 
        (Just g) -> Result $ show (toValue g :: DAO.Graph Identifier)
        _ -> Error "Could not find a matching graph from constraints"

data GraphParamDescription = GraphParamDescription
    { universe :: [DAO.Identifier]
    , constraints :: [DAO.GraphConstraint]
    } deriving (Show,Read)