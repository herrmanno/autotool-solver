module Tasks.Relations (runTask) where

import Prelude hiding ((+), (-), (*))
import Data.List (intercalate)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Data.Set (Set)
import Autotool.Data.LazyTree (isOp0, Op(..), showTree, eval, trees)
import Autotool.Parser.Relation ( parseRelation )
import Autotool.Data.RelOp ((+), (&), (-), (*))
import Autotool.Solver.Relations (solve)

runTask :: String -> String
runTask input = case parseSetDescription input of
    (Right d@(RelationDescription ops t)) -> showTree $ solve ops t
    (Left err) -> show err

data RelationDescription = RelationDescription
    { ops :: [Op (Set (Integer,Integer))]
    , target :: Set (Integer,Integer)
    }

instance Show RelationDescription where
    show (RelationDescription ops t) =
        let target = "Target: " ++ show t
            sets = "Relations: " ++ intercalate ", " (map (\cnst -> show cnst ++ " = " ++ show (eval cnst [])) $ filter isOp0 ops)
            ops' = "Operators: " ++ intercalate ", " (map show $ filter (not . isOp0) ops)
        in unlines [ops', sets, target]

parseSetDescription :: String -> Either P.ParseError RelationDescription
parseSetDescription = P.parse parse ""
    where
        parse = let
            f ops ss = RelationDescription (ss ++ ops)
            in f <$> parseOPs <*> parseRelations <*> parseTarget
        parseOPs = (P.string "#OPS" >> P.spaces) *> P.many1 parseOp <* P.spaces
        parseOp = parseOpCup <|> parseOpDiff <|> parseOpCap <|> parseOpCompose
        parseOpCup = P.char '+' >> P.spaces >> return (+)
        parseOpDiff = P.char '-' >> P.spaces >> return (-)
        parseOpCap = P.char '&' >> P.spaces >> return (&)
        parseOpCompose = P.string "." >> P.spaces >> return (*)
        parseTarget = (P.string "#TARGET" >> P.spaces) *> parseRelation <* P.spaces
        parseRelations = (P.string "#RELATIONS" >> P.spaces) *> P.many1 parseConst <* P.spaces
        parseConst = Op0 <$> (P.many1 P.alphaNum <* parseEq) <*> (parseRelation <* P.spaces)
        parseEq = P.spaces >> P.char '=' >> P.spaces