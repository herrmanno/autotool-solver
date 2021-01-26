module Tasks.Sets (runTask) where

import Prelude hiding ((+), (-))
import Data.List (intercalate)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Autotool.Data.LazyTree (isOp0, Op(..), showTree, eval, trees)
import Autotool.Data.NestedSet (toStr, NSet, ø)
import Autotool.Parser.NestedSet ( parseSet )
import Autotool.Data.SetOp ((+), (&), (-), pow)
import Autotool.Solver.Sets (solve)

runTask :: String -> String
runTask input = case parseSetDescription input of
    (Right d@(SetDescription ops t)) -> showTree $ solve ops t
    (Left err) -> show err

data SetDescription = SetDescription { ops :: [Op (NSet Integer)] , target :: NSet Integer }

instance Show SetDescription where
    show (SetDescription ops t) =
        let target = "Target: " ++ toStr t
            sets = "Sets: " ++ intercalate ", " (map (\cnst -> show cnst ++ " = " ++ toStr (eval cnst [])) $ filter isOp0 ops)
            ops' = "Operators: " ++ intercalate ", " (map show $ filter (not . isOp0) ops)
        in unlines [ops', sets, target]

parseSetDescription :: String -> Either P.ParseError SetDescription
parseSetDescription = P.parse parse ""
    where
        parse = let
            f ops ss = SetDescription (ss ++ ops)
            in f <$> parseOPs <*> parseSets <*> parseTarget
        parseOPs = (P.string "#OPS" >> P.spaces) *> P.many1 parseOp <* P.spaces
        parseOp = parseOpCup <|> parseOpDiff <|> parseOpCap <|> parseOpPow
        parseOpCup = P.char '+' >> P.spaces >> return (+)
        parseOpDiff = P.char '-' >> P.spaces >> return (-)
        parseOpCap = P.char '&' >> P.spaces >> return (&)
        parseOpPow = P.string "pow" >> P.spaces >> return pow
        parseTarget = (P.string "#TARGET" >> P.spaces) *> parseSet <* P.spaces
        parseSets = (P.string "#SETS" >> P.spaces) *> P.many1 parseConst <* P.spaces
        parseConst = Op0 <$> (P.many1 P.alphaNum <* parseEq) <*> (parseSet <* P.spaces)
        parseEq = P.spaces >> P.char '=' >> P.spaces