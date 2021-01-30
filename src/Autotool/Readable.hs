module Autotool.Readable where

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, char, skipSpaces,skipSpaces, munch1 )
import Data.Char (isDigit)

class Readable a where
    -- | A ReadP parser for type `a`
    readP :: ReadP a
    -- | Runs a reader of type `a` on a string
    runReadP :: String -> a
    runReadP = fst . head . readP_to_S readP

spaces :: ReadP ()
spaces = skipSpaces

comma :: ReadP ()
comma = char ',' >> spaces

openPar :: ReadP ()
openPar = char '(' >> spaces

closePar :: ReadP ()
closePar = char ')' >> spaces

openBracket :: ReadP ()
openBracket = char '[' >> spaces

closeBracket :: ReadP ()
closeBracket = char ']' >> spaces

openBracelet :: ReadP ()
openBracelet = char '{' >> spaces

closeBracelet :: ReadP ()
closeBracelet = char '}' >> spaces

instance Readable Int where
    readP = read <$> munch1 isDigit

instance Readable Integer where
    readP = read <$> munch1 isDigit