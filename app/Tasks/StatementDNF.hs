module Tasks.StatementDNF (runTask) where

import Autotool.Data.LazyTree (showTree)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), Interpretation)
import Autotool.Solver.StatementDNF (solve)


runTask :: String -> String
runTask s = showTree $ tree $ solve stm
    where
        desc = read s :: StatementDNFDescription
        stm = toValue (statement desc) :: Statement

newtype StatementDNFDescription = StatementDNFDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)