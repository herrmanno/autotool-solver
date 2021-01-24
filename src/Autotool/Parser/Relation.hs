module Autotool.Parser.Relation ( parseIntRelation ) where

import Data.Either (fromRight)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Data.Set as S

parseIntRelation :: String -> S.Set (Integer,Integer)
parseIntRelation = fromRight undefined . parseIntRelation'

parseIntRelation' :: String -> Either P.ParseError (S.Set (Integer,Integer))
parseIntRelation' = P.parse parseSet ""
    where
        parseSet = P.try parseEmpty <|> parseNonEmpty
        parseEmpty = P.string "{}" >> return S.empty
        parseNonEmpty = P.between (P.char '{') (P.char '}') parseValues
        parseValues = S.fromList <$> P.sepBy1 parseTuple sep
        parseTuple = (,) <$> (P.char '(' *> parseInt) <*> (sep *> parseInt <* P.char ')')
        parseInt = (read :: [Char] -> Integer) <$> (P.spaces *> P.many1 P.digit <* P.spaces)
        sep = P.spaces >> P.char ',' >> P.spaces