{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Autotool.Readable where

import Control.Applicative (Alternative((<|>)))
import Text.ParserCombinators.ReadP
    ( ReadP
    , readP_to_S
    , char
    , string
    , skipSpaces
    , between
    , munch1
    , sepBy )
import Data.Char (isDigit)

class Readable a where
    -- | A ReadP parser for type `a`
    readP :: ReadP a
    -- | Runs a reader of type `a` on a string
    runReadP :: String -> a
    runReadP = fst . head . readP_to_S readP

spaces :: ReadP ()
spaces = skipSpaces

spaced :: ReadP a -> ReadP a
spaced r = spaces *> r <* spaces

spacedString :: String -> ReadP String
spacedString str = spaced $ string str

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
    readP = read <$> value
        where
            value = number <|> paraNumber
            number = munch1 isDigit
            paraNumber = between openPar closePar value

instance Readable Integer where
    readP = read <$> value
        where
            value = number <|> paraNumber
            number = munch1 isDigit
            paraNumber = between openPar closePar value

instance (Readable a) => Readable [a] where
    readP = spaces *> between openBracket closeBracket readList
        where
            readList = readP `sepBy` comma

instance (Readable a, Readable b) => Readable (a,b) where
    readP = between openPar closePar readTuple
        where
            readTuple = (,) <$> readValueA <*> (comma *> readValueB)
            readValueA = readP <* spaces
            readValueB = readP <* spaces

instance (Readable a, Readable b, Readable c) => Readable (a,b, c) where
    readP = between openPar closePar readTuple
        where
            toTuple a b c = (a,b,c)
            readTuple = toTuple <$> readValueA <*> (comma *> readValueB) <*> (comma *> readValueC)
            readValueA = readP <* spaces
            readValueB = readP <* spaces
            readValueC = readP <* spaces