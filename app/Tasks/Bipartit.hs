module Tasks.Bipartit (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Bipartit (solve)


runTask :: String -> String
runTask s = show $ solve g
    where
        desc = read s :: BipartitGraphsDescription
        g = toValue (graph desc) :: G.Graph Int

data BipartitGraphsDescription = BipartitGraphsDescription
    { graph :: DAO.Graph Int
    } deriving (Show,Read)