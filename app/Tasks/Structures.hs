module Tasks.Structures (runTask) where

import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import Autotool.Data.LazyTree (showFnTree)
import Autotool.DAO.Structure ( Struktur(..) )
import Autotool.Solver.Structures (solve)


runTask :: String -> String
runTask = showFnTree . solve . structs . read

newtype StructureDescription = StructureDescription { structs :: [Struktur] } deriving (Show,Read)