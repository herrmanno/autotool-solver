module Tasks.Sets (runTask) where

import Prelude hiding ((+), (-))
import qualified Data.Map as M
import Autotool.DAO
import Autotool.DAO.NestedSet (NestedSet, SetOp)
import Autotool.DAO.Map (Map)
import Autotool.DAO.Identifier (Identifier)
import Autotool.Data.LazyTree (Op, mkOp0, showTree)
import Autotool.Data.NestedSet (NSet)
import Autotool.Solver.Sets (solve, solveP)

-- TODO: read limit and parallel flag from description
runTask :: String -> String
runTask input = showTree $ solveP (sops ++ ops) t
    where
        desc = read input :: SetDescription
        ops = (map toValue $ operators desc) :: [Op () (NSet Int)]
        sops = map (\(name,set) -> mkOp0 (show name) (toValue set)) (sets desc) :: [Op () (NSet Int)]
        t = (toValue $ target desc) :: NSet Int

data SetDescription = SetDescription
    { operators :: [SetOp Int]
    , sets :: [(Identifier, NestedSet Int)]
    , target :: NestedSet Int
    } deriving (Show,Read)