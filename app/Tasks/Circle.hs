module Tasks.Circle (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Circle (solve)


runTask :: String -> String
runTask s = show $ solve l g
    where
        desc = read s :: CircleDescription
        l = Tasks.Circle.length desc
        g = toValue (graph desc) :: G.Graph Int

data CircleDescription = CircleDescription
    { graph :: DAO.Graph Int
    , length :: Int
    } deriving (Show,Read)