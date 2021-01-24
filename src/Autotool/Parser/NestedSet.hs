module Autotool.Parser.NestedSet ( parseIntSet ) where

import Data.Either (fromRight)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Data.Set as S
import Autotool.Data.NestedSet ( NSet, T, ø, value, set, unwrap )

parseIntSet :: String -> NSet Integer
parseIntSet = fromRight undefined . parseIntSet'

parseIntSet' :: String -> Either P.ParseError (NSet Integer)
parseIntSet' = P.parse parseSet ""
    where
        parseSet = unwrap <$> (P.try parseEmpty <|> parseNonEmpty)
        parseEmpty = P.spaces >> P.between (P.char '{') (P.char '}') P.spaces >> P.spaces >> return (set ø :: T Integer)
        parseNonEmpty = P.spaces *> P.between (P.char '{') (P.char '}') parseValues <* P.spaces
        parseValues = set . S.fromList <$> P.sepBy1 parseValue sep
        parseValue = P.try parseInt <|> P.try parseEmpty <|> parseNonEmpty
        parseInt = value . (read :: [Char] -> Integer) <$> (P.spaces *> P.many1 P.digit <* P.spaces)
        sep = P.spaces >> P.char ',' >> P.spaces