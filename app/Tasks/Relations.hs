module Tasks.Relations (runTask) where

import qualified Data.Set as S
import Autotool.DAO (toValue)
import Autotool.DAO.Set (Set)
import Autotool.DAO.Relation (RelOp)
import Autotool.DAO.Identifier (Identifier)
import Autotool.Data.LazyTree (isOp0, Op(..), showTree, eval, trees)
import Autotool.Solver.Relations (solveP)

runTask :: String -> String
runTask input = showTree $ solveP (rops ++ ops) t
    where
        desc = read input :: RelationDescription
        ops = (map toValue $ operators desc) :: [Op (S.Set (Int,Int))]
        rops = map (\(name,rel) -> Op0 (show name) (toValue rel)) (relations desc) :: [Op (S.Set (Int,Int))]
        t = (toValue $ target desc) :: S.Set (Int,Int)

data RelationDescription = RelationDescription
    { operators :: [RelOp Int]
    , relations :: [(Identifier, Set (Int, Int))]
    , target :: Set (Int,Int)
    } deriving (Show,Read)