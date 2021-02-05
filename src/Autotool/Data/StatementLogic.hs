module Autotool.Data.StatementLogic
    ( Statement(..)
    , Interpretation
    , universe
    , interpretations
    , freeVar
    , (&&)
    , (||)
    , (!)
    ) where

import Prelude hiding ((&&), (||))
import qualified Prelude as Pre ((&&), (||))
import qualified Data.Map as M
import Data.Tree ( Tree(Node), foldTree )
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Autotool.Readable ( closePar, openPar, Readable(readP), spacedString, spaced )
import Autotool.Data.LazyTree (isOp0, Op, mkOp0C, mkOp1, mkOp2)

type Interpretation = M.Map Char Bool

newtype Statement = Statement { tree :: Tree (Op Interpretation Bool) } deriving (Show)

universe :: Statement -> [Char]
universe = map head . foldTree f . tree
    where f leaf children
            | isOp0 leaf = [show leaf]
            | otherwise = concat children

interpretations :: [Char] -> [Interpretation]
interpretations [x] = [M.singleton x False, M.singleton x True]
interpretations (x:xs) = do
    i <- interpretations xs
    [M.insert x False i, M.insert x True i]

-- operations

freeVar :: Char -> Op Interpretation Bool
freeVar c = mkOp0C [c] (M.! c)

(!) :: Op Interpretation Bool
(!) = mkOp1 "!" not

(&&) :: Op Interpretation Bool
(&&) = mkOp2 "&&" True (Pre.&&)

(||) :: Op Interpretation Bool
(||) = mkOp2 "||" True (Pre.||)


-- Read(able) instance

instance Read Statement where
    readsPrec _ = P.readP_to_S readP

instance Readable Statement where
    readP = Statement <$> expr0
        where
            expr0 = P.chainl1 expr1 (spacedString "||" $> node2 (||))
            expr1 = P.chainl1 expr2 (spacedString "&&" $> node2 (&&))
            expr2 = term <|> node1 (!) <$> (spacedString "!" *> term)
            term = var <|> spaced (openPar *> expr0 <* closePar)
            var = node0 . freeVar <$> spaced (P.satisfy (`elem` ['a'..'z']))
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]
