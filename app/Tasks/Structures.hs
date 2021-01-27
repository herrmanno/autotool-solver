module Tasks.Structures (runTask) where

import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import Autotool.Data.LazyTree (showFnTree)
import Autotool.Data.Structure ( Binding(..), Readable(..), Struktur )
import Autotool.Solver.Structures (solve)


runTask :: String -> String
runTask = showFnTree . solve . structs . readDescription
    where readDescription = runReadP :: (String -> StructureDescription)

data StructureDescription = StructureDescription { structs :: [Struktur] } deriving (Show)

instance Readable StructureDescription where
    readP = StructureDescription <$> (parseHeader *> parseStructures)
        where
            parseStructures = P.manyTill parseStructure P.eof
            parseHeader = P.string "#STRUCTURES" >> P.skipSpaces
            parseStructure = (\(Binding (_, v)) -> v) <$> (readP :: ReadP (Binding Struktur)) <* P.skipSpaces