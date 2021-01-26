module Autotool.Parser.NestedSet ( parseIntSet, parseSet ) where

import Data.Either (fromRight)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Data.Set as S
import Autotool.Data.NestedSet ( NSet, T, ø, value, set, unwrap )

-- TODO: clear naming of Parse parser instance and string returning parser

parseIntSet :: String -> NSet Integer
parseIntSet = fromRight undefined . P.parse parseSet ""

parseSet :: (Monad a) => P.ParsecT String u a (NSet Integer)
parseSet = unwrap <$> (P.try parseEmpty <|> parseNonEmpty)
    where
        parseEmpty = P.spaces >> P.between (P.char '{') (P.char '}') P.spaces >> P.spaces >> return (set ø :: T Integer)
        parseNonEmpty = P.spaces *> P.between (P.char '{') (P.char '}') parseValues <* P.spaces
        parseValues = set . S.fromList <$> P.sepBy1 parseValue sep
        parseValue = P.try parseInt <|> P.try parseEmpty <|> parseNonEmpty
        parseInt = value . (read :: [Char] -> Integer) <$> (P.spaces *> P.many1 P.digit <* P.spaces)
        sep = P.spaces >> P.char ',' >> P.spaces