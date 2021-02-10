{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Autotool.DAO.Statement (Statement, StatementOp) where

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
import Autotool.Data.LazyTree (Tree(Node),  Op, showTree)
import Autotool.Data.StatementLogic ((&&), (||), (!), (-->), (<-->), var, true, false)
import qualified  Autotool.Data.StatementLogic as SL
import Autotool.DAO.Identifier (Identifier, mkId)
import Autotool.DAO.Map (Map, mapToFM)

type Statement = Tree (Op SL.Interpretation Bool) -- TODO: use newtype instead

instance {-# OVERLAPS #-} Show Statement where
    show = showTree

instance {-# OVERLAPS #-} Read Statement where
    readsPrec _ = P.readP_to_S readP

instance Readable Statement where
    readP = expr0
        where
            expr0 = P.chainl1 expr1 (spacedString "<->" $> node2 (<-->))
            expr1 = P.chainl1 expr2 (spacedString "->" $> node2 (-->))
            expr2 = P.chainl1 expr3 (spacedString "||" $> node2 (||))
            expr3 = P.chainl1 expr4 (spacedString "&&" $> node2 (&&))
            expr4 = term <|> node1 (!) <$> (spacedString "!" *> term)
            term = atom <|> spaced (openPar *> expr0 <* closePar)
            atom = variable <|> t <|> f
            t = spaced (P.string "true") $> node0 true
            f = spaced (P.string "false") $> node0 false
            variable = node0 . var <$> spaced (P.satisfy (`elem` ['a'..'z']))
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]

instance (DAO SL.Statement) Statement where
    toValue = SL.Statement

instance (DAO (Map Identifier Bool)) SL.Interpretation where
    toValue = mapToFM . M.mapKeys mkId


-- OPERATORS

{- Relation OPERATIONS -}

data StatementOp = OpTrue | OpFalse | OpConjunction | OpDisjunction | OpNegation | OpImplication | OpEquivalence

instance Show StatementOp where
    show OpTrue = "true"
    show OpFalse = "false"
    show OpConjunction = "&&"
    show OpDisjunction = "||"
    show OpNegation = "!"
    show OpImplication = "->"
    show  OpEquivalence = "<->"

instance Read StatementOp where
    readsPrec _ = P.readP_to_S readP

instance Readable StatementOp where
    readP = readTrue <|> readFalse <|> readConjunction <|> readDisjunction <|> readNegation <|> readImplication <|> readEquivalence
        where
            readTrue = spaced (P.string "true") $> OpTrue
            readFalse = spaced (P.string "false") $> OpFalse
            readConjunction = spaced (P.string "&&") $> OpConjunction
            readDisjunction = spaced (P.string "||") $> OpDisjunction
            readNegation = spaced (P.string "!") $> OpNegation
            readImplication = spaced (P.string "->") $> OpImplication
            readEquivalence = spaced (P.string "<->") $> OpEquivalence

instance (DAO SL.StatementOp) StatementOp where
    toValue OpTrue = true
    toValue OpFalse  = false
    toValue OpConjunction  = (&&)
    toValue OpDisjunction  = (||)
    toValue OpNegation  = (!)
    toValue OpImplication = (-->)
    toValue OpEquivalence = (<-->)