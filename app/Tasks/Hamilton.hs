module Tasks.Hamilton (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Hamilton (solve)


runTask :: String -> String
runTask s = show $ solve g
    where
        desc = read s :: HamiltonDescription
        g = toValue (graph desc) :: G.Graph Int

data HamiltonDescription = HamiltonDescription { graph :: DAO.Graph Int } deriving (Show,Read)