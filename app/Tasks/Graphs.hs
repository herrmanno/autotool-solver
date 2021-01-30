module Tasks.Graphs (runTask) where

import Autotool.Data.LazyTree (showTree, Op)
import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph, GraphConst, GraphOp )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Graphs (solve)


runTask :: String -> String
runTask s = showTree $ solve (consts ++ operators) t
    where
        desc = read s :: GraphsDescription
        t = toValue $ target desc :: G.Graph Int
        operators = map toValue $ ops desc :: [Op (G.Graph Int)]
        consts = map toValue $ graphs desc :: [Op (G.Graph Int)]

data GraphsDescription = GraphsDescription
    { target :: DAO.Graph Int
    , graphs :: [DAO.GraphConst]
    , ops :: [DAO.GraphOp Int]
    } deriving (Show,Read)