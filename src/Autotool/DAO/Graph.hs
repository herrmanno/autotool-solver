{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Graph
    ( Graph
    , Kante
    , GraphConst
    , GraphOp
    ) where

import Prelude hiding ((+), (*))
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import Autotool.DAO (DAO(..))
import Autotool.DAO.Set (Set)
import Autotool.Readable (spaces, Readable(..))
import qualified Autotool.Data.Graph as G
import Autotool.Data.GraphOp ((+),(*),co)
import Autotool.Data.LazyTree (Op, mkOp0)

{- GRAPH -}

data Graph a = Graph
    { knoten :: Set a
    , kanten :: Set (Kante a)
    } deriving (Show,Read)

instance (Ord a) => (DAO (G.Graph a)) (Graph a) where
    toValue (Graph knoten kanten) = (toValue knoten, S.map toValue (toValue kanten :: S.Set (Kante a)))

{- GRAPH -> KANTE -}

data Kante a = Kante a a deriving (Eq,Ord)

instance (Show a) => Show (Kante a) where
    show (Kante a b) = "kante " ++ show a ++ " " ++ show b

instance (Readable a) => Read (Kante a) where
    readsPrec _ = P.readP_to_S readP

instance (Readable a) => Readable (Kante a) where
    readP = Kante <$> (readHeader *> readValue) <*> readValue
        where
            readHeader = P.string "kante" >> P.skipSpaces
            readValue = (readP :: P.ReadP a) <* P.skipSpaces

instance (DAO (a,a)) (Kante a) where
    toValue (Kante a b) = (a,b)



{- KNOWN GRAPHS (Constants) -}

data GraphConst = GraphConstI Int | GraphConstK Int | GraphConstP Int | GraphConstC Int

instance Show GraphConst where
    show (GraphConstI n) = 'I':show n
    show (GraphConstK n) = 'K':show n
    show (GraphConstP n) = 'P':show n
    show (GraphConstC n) = 'C':show n

instance Read GraphConst where
    readsPrec _ = P.readP_to_S readP

instance Readable GraphConst where
    readP = readI <|> readK <|> readP <|> readC
        where
            readI = GraphConstI <$> (spaces *> P.char 'I' *> readInt) <* spaces
            readK = GraphConstK <$> (spaces *> P.char 'K' *> readInt) <* spaces
            readP = GraphConstP <$> (spaces *> P.char 'P' *> readInt) <* spaces
            readC = GraphConstC <$> (spaces *> P.char 'C' *> readInt) <* spaces
            readInt = P.readS_to_P (reads :: ReadS Int) -- TODO: use Int's readP instead

instance (DAO (G.Graph Int)) GraphConst where
    toValue (GraphConstI n) = G.mkI n
    toValue (GraphConstK n) = G.mkK n
    toValue (GraphConstP n) = G.mkP n
    toValue (GraphConstC n) = G.mkC n

instance (DAO (Op c (G.Graph Int))) GraphConst where
    toValue g@(GraphConstI n) = mkOp0 ('I':show n) (toValue g)
    toValue g@(GraphConstK n) = mkOp0 ('K':show n) (toValue g)
    toValue g@(GraphConstP n) = mkOp0 ('P':show n) (toValue g)
    toValue g@(GraphConstC n) = mkOp0 ('C':show n) (toValue g)



{- GRAPH OPERATIONS -}

data GraphOp a = OpSum | OpJunction | OpComplement

instance Show (GraphOp a) where
    show OpSum = "+"
    show OpJunction = "*"
    show OpComplement = "co"

instance Read (GraphOp a) where
    readsPrec _ = P.readP_to_S readP

instance Readable (GraphOp a) where
    readP = readSum <|> readJunction <|> readComplement
        where
            readSum = (spaces >> P.char '+' >> spaces) $> OpSum
            readJunction = (spaces >> P.char '*' >> spaces) $> OpJunction
            readComplement = (spaces >> P.string "co" >> spaces) $> OpComplement

instance (Num a, Ord a) => (DAO (Op c (G.Graph a))) (GraphOp a) where
    toValue OpSum = (+)
    toValue OpJunction = (*)
    toValue OpComplement = co