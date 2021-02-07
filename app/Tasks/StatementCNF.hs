module Tasks.StatementCNF (runTask) where

import Autotool.Data.LazyTree (showTree)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO
import Autotool.Data.StatementLogic (Statement(..), Interpretation)
import Autotool.Solver.StatementCNF (solve)


runTask :: String -> String
runTask s = showTree $ tree $ solve stm
    where
        desc = read s :: StatementCNFDescription
        stm = toValue (statement desc) :: Statement

newtype StatementCNFDescription = StatementCNFDescription
    { statement :: DAO.Statement
    } deriving (Show,Read)