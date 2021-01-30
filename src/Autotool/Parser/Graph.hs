{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.Parser.Graph where

import qualified Text.Read as T
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as P
import Autotool.Data.Structure (Readable(..))
import Autotool.Data.Graph (kante,  Graph, mkGraph )

instance (Read a, Ord a) => Readable (Graph a) where
    readP = readGraph
        where
            readGraph = mkGraph <$> (readHeader *> readVertices) <*> readEdges
            readHeader = P.skipSpaces >> P.string "mkGraph" >> P.skipSpaces
            readVertices = P.readS_to_P (T.readList :: ReadS [a])
            readEdges = P.between openBr closeBr (P.sepBy readKante comma)
            readKante = kante <$> (P.string "kante" >> P.skipSpaces *> readValue) <*> readValue
            readValue = P.readS_to_P (reads :: ReadS a) <* P.skipSpaces
            comma = P.skipSpaces >> P.char ',' >> P.skipSpaces
            openBr = P.skipSpaces >> P.char '[' >> P.skipSpaces
            closeBr = P.skipSpaces >> P.char ']' >> P.skipSpaces