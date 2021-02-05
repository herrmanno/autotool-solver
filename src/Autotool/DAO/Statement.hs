{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Autotool.DAO.Statement (Statement) where

import Prelude hiding ((&&), (||))
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import qualified Data.Map as M
import Autotool.Readable
    ( closePar,
      openPar,
      Readable(readP),
      spacedString,
      spaced,
      Readable(..) )
import Autotool.DAO ( DAO(..) )
import Autotool.Data.LazyTree (Tree(Node),  Op)
import qualified  Autotool.Data.StatementLogic as SL
import Autotool.DAO.Identifier (Identifier, mkId)
import Autotool.DAO.Map (Map, mapToFM)

type Statement = Tree (Op SL.Interpretation Bool)

instance {-# OVERLAPS #-} Read Statement where
    readsPrec _ = P.readP_to_S readP

instance Readable Statement where
    readP = expr0
        where
            expr0 = P.chainl1 expr1 (spacedString "||" $> node2 (SL.||))
            expr1 = P.chainl1 expr2 (spacedString "&&" $> node2 (SL.&&))
            expr2 = term <|> node1 (SL.!) <$> (spacedString "!" *> term)
            term = var <|> spaced (openPar *> expr0 <* closePar)
            var = node0 . SL.freeVar <$> spaced (P.satisfy (`elem` ['a'..'z']))
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]

instance (DAO SL.Statement) Statement where
    toValue = SL.Statement

instance (DAO (Map Identifier Bool)) SL.Interpretation where
    toValue = mapToFM . M.mapKeys mkId