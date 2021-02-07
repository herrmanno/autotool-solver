module Tasks.Bipartit (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import Autotool.DAO.Set (mkSet)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Bipartit (solve)


runTask :: String -> String
runTask s = show $ mkSet $ solve g
    where
        desc = read s :: BipartitGraphsDescription
        g = toValue (graph desc) :: G.Graph Int

newtype BipartitGraphsDescription = BipartitGraphsDescription
    { graph :: DAO.Graph Int
    } deriving (Show,Read)