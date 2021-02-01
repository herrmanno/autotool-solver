module Tasks.Sets (runTask) where

import Prelude hiding ((+), (-))
import qualified Data.Map as M
import Autotool.DAO
import Autotool.DAO.NestedSet (NestedSet, SetOp)
import Autotool.DAO.Map (Map)
import Autotool.DAO.Identifier (Identifier)
import Autotool.Data.LazyTree (Op(..), showTree)
import Autotool.Data.NestedSet (NSet)
import Autotool.Solver.Sets (solve)

runTask :: String -> String
runTask input = showTree $ solve (sops ++ ops) t
    where
        desc = read input :: SetDescription
        ops = (map toValue $ operators desc) :: [Op (NSet Int)]
        sops = map (\(name,set) -> Op0 (show name) (toValue set)) (sets desc) :: [Op (NSet Int)]
        t = (toValue $ target desc) :: NSet Int

data SetDescription = SetDescription
    { operators :: [SetOp Int]
    , sets :: [(Identifier, NestedSet Int)]
    , target :: NestedSet Int
    } deriving (Show,Read)