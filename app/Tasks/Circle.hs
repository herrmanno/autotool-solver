module Tasks.Circle (runTask) where

import Autotool.DAO
import qualified Autotool.DAO.Graph as DAO ( Graph )
import Autotool.DAO.Set (mkSet)
import qualified Autotool.Data.Graph as G
import Autotool.Solver.Circle (solve)


runTask :: String -> String
runTask s = unlines $ map (show . mkSet) $ solve l g
    where
        desc = read s :: CircleDescription
        l = Tasks.Circle.length desc
        g = toValue (graph desc) :: G.Graph Int

data CircleDescription = CircleDescription
    { graph :: DAO.Graph Int
    , length :: Int
    } deriving (Show,Read)