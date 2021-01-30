{-# LANGUAGE ScopedTypeVariables #-}

-- FIXME: split into DAO- and Data type
module Autotool.Data.Structure where

import qualified Text.Read as T
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)

class Readable a where
    readP :: ReadP a
    runReadP :: String -> a
    runReadP = fst . head . P.readP_to_S readP

newtype Identifier = Id Char deriving (Ord,Eq)

instance Show Identifier where
    show (Id a) = [a]

instance Readable Identifier where
    readP = Id <$> (P.skipSpaces *> char <* P.skipSpaces)
        where
            char = P.satisfy isChar
            isChar c = c `elem` ['A'..'Z'] || c `elem` ['a'..'z']

instance Read Identifier where
    readsPrec n = P.readP_to_S readP


newtype Binding a = Binding (Identifier, a)

instance (Show a) => Show (Binding a) where
    show (Binding (i, a)) = show i ++ " = " ++ show a

instance (Read a) => Readable (Binding a) where
    readP = Binding <$> parseTuple
        where
            parseTuple = (,) <$> (readId <* readEq) <*> readValue
            readId = readP :: ReadP Identifier
            readValue = P.readS_to_P (reads :: ReadS a) <* P.skipSpaces
            readEq = P.char '=' >> P.skipSpaces


instance (Read a) => Read (Binding a) where
    readsPrec n = P.readP_to_S readP


data Function a = Fn0 (Set a) | Fn1 (Set (a,a)) | Fn2 (Set (a,a,a)) deriving (Eq,Ord)

maplet :: (Ord a) => Function a -> ([a] -> a)
maplet (Fn0 set) = let s = toSet set; [v] = S.toList s in const v
maplet (Fn1 set) = let s = toSet set; m = M.fromList $ S.toList s in (M.!) m . head
maplet (Fn2 set) = let s = toSet set; m = M.fromList $ map (\(a,b,c) -> ((a,b),c)) $ S.toList s in (M.!) m . (\[a,b] -> (a,b))

instance (Show a) => Show (Function a) where
    show (Fn0 s) = show s
    show (Fn1 s) = show s
    show (Fn2 s) = show s

instance (Read a, Ord a) => Readable (Function a) where
    readP = parseFn0 +++ parseFn1 +++ parseFn2
        where
            parseFn0 = Fn0 <$> (P.readS_to_P reads :: ReadP (Set a))
            parseFn1 = Fn1 <$> (P.readS_to_P reads :: ReadP (Set (a,a)))
            parseFn2 = Fn2 <$> (P.readS_to_P reads :: ReadP (Set (a,a,a)))

instance (Read a, Ord a) => Read (Function a) where
    readsPrec n = P.readP_to_S readP


data Set a = Set (S.Set a) | BrSet (S.Set a) deriving (Eq, Ord)

instance (Show a) => Show (Set a) where
    show (Set s) = "mkSet [" ++ intercalate "," (map show $ S.toList s) ++ "]"
    show (BrSet s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"

instance (Read a, Ord a) => Readable (Set a) where
    readP = P.skipSpaces *> (readSet +++ readBrSet) <* P.skipSpaces
        where
            readSet = Set . S.fromList <$> ((P.string "mkSet" >> P.skipSpaces) *> P.between openBracket closeBracket readList)
            readBrSet = BrSet . S.fromList <$> P.between openBracelet closeBracelet readList
            readList = P.sepBy readEl comma <* P.skipSpaces
            readEl = P.readS_to_P (reads :: ReadS a) <* P.skipSpaces
            openBracket = P.char '[' >> P.skipSpaces
            comma = P.char ',' >> P.skipSpaces
            closeBracket = P.char ']' >> P.skipSpaces
            openBracelet = P.char '{' >> P.skipSpaces
            closeBracelet = P.char '}' >> P.skipSpaces
instance (Read a, Ord a) => Read (Set a) where
    readsPrec n = P.readP_to_S readP


mkSet :: (Ord a) => [a] -> Set a
mkSet = Set . S.fromList

toSet :: Set a -> S.Set a
toSet (Set s) = s
toSet (BrSet s) = s


newtype Map a k = Map (M.Map a k) deriving (Eq, Ord)

instance (Show a, Show k) => Show (Map a k) where
    show (Map m) = "listToFM [" ++ intercalate "," (map show $ M.toList m) ++ "]"


instance (Read a, Read k, Ord k) => Readable (Map k a) where
    readP =  Map . M.fromList <$> (P.string "listToFM" >> P.skipSpaces *> parseList)
        where parseList = P.readS_to_P T.readList :: ReadP [(k,a)]

instance (Read a, Read k, Ord k) => Read (Map k a) where
    readsPrec n = P.readP_to_S readP

listToFM :: (Ord a) => [(a,b)] -> Map a b
listToFM = Map . M.fromList


data Struktur = Struktur
    { _universum :: Set Int
    , _predicate :: Map Identifier (Function Int)
    , _functions :: Map Identifier (Function Int)
    } deriving (Eq)

functions :: Struktur -> M.Map Identifier (Function Int)
functions Struktur { _functions = (Map f) } = f

instance Show Struktur where
    show Struktur { _universum = u, _functions = f } = "Struktur { universum = " ++ show u ++ ", functions = " ++ show f ++ " }"

instance Readable Struktur where
    readP = parseHeader >> P.between bracketOpen bracketClose parseBody <* P.skipSpaces
        where
            parseHeader = P.string "Struktur" >> P.skipSpaces
            parseBody = Struktur <$> parseUniversum <*> (comma *> parsePredicates) <*> (comma *> parseFunctions)
            parseUniversum = P.string "universum" >> P.skipSpaces >> P.char '=' >> P.skipSpaces >> parseUniversumSet <* P.skipSpaces
            parseUniversumSet = readP :: ReadP (Set Int)
            parsePredicates = P.string "predicates" >> P.skipSpaces >> P.char '=' >> P.skipSpaces >> parsePredicatesMap <* P.skipSpaces
            parsePredicatesMap = readP :: ReadP (Map Identifier (Function Int))
            parseFunctions = P.string "functions" >> P.skipSpaces >> P.char '=' >> P.skipSpaces >> parseFunctionsMap <* P.skipSpaces
            parseFunctionsMap = readP :: ReadP (Map Identifier (Function Int))
            bracketOpen = P.char '{' >> P.skipSpaces
            bracketClose = P.char '}' >> P.skipSpaces
            comma = P.char ',' >> P.skipSpaces

instance Read Struktur where
    readsPrec n = P.readP_to_S readP