module Tasks.Isomorphism (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Isomorphism (solve)


runTask :: String -> String
runTask s = case solve g h of
    (Just iso) -> show iso
    _ -> "ERROR: Cannot find an isomorphism from g to h"
    where
        desc = read s :: IsomorphismDescription
        g = toValue (graph1 desc) :: G.Graph Int
        h = toValue (graph2 desc) :: G.Graph Int

data IsomorphismDescription = IsomorphismDescription
    { graph1 :: DAO.Graph Int
    , graph2 :: DAO.Graph Int
    } deriving (Show,Read)