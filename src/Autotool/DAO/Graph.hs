{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autotool.DAO.Graph
    ( Graph
    , Kante
    , GraphConst
    , GraphOp
    , GraphConstraint
    ) where

import Prelude hiding ((+), (*))
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import Autotool.DAO (DAO(..))
import Autotool.DAO.Set (Set, mkSet)
import Autotool.Readable (Readable(..), spaces, equals, openPar, closePar, number, comma)
import qualified Autotool.Data.Graph as G
import Autotool.Data.GraphOp ((+),(*),co)
import Autotool.Data.LazyTree (Op, mkOp0)
import Autotool.DAO.Identifier (Identifier, fromId)
import Data.Bifunctor (Bifunctor(bimap))

{- GRAPH -}

data Graph a = Graph
    { knoten :: Set a
    , kanten :: Set (Kante a)
    } deriving (Show,Read)

instance (Ord a, Ord b, ((DAO b) a)) => (DAO (G.Graph b)) (Graph a) where
    toValue (Graph knoten kanten) = (vs, es)
        where
            vs = toValue (toValue knoten :: S.Set a) :: S.Set b
            es = S.map (bimap toValue toValue) (toValue (toValue kanten :: S.Set (Kante a)) :: S.Set (a,a))

instance (Ord b, ((DAO b) a)) => (DAO (Graph b)) (G.Graph a) where
    toValue (vs, es) =
        let vs' = mkSet $ toValue (S.toList vs)
            es' = mkSet $ S.toList $ S.map (uncurry Kante) $ S.map (bimap toValue toValue) es
        in Graph vs' es'

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


{- GRAPH CONSTRAINTS -}

data GraphConstraint
    = Vertices Int
    | Edges Int
    | MaxDegree Int
    | MaxClique Int
    | Edge Identifier Identifier
    | Degree Identifier Int
    | Not GraphConstraint
    deriving (Show)

instance Read GraphConstraint where
    readsPrec _ = P.readP_to_S readP

instance Readable GraphConstraint where
    readP = readVertices <|> readEdges <|> readMaxdegree <|> readMaxclique <|> readEdge <|> readDegree <|> readNot
        where
            readVertices = Vertices <$> ((spaces >> P.string "vertices" >> spaces >> equals) *> number)
            readEdges = Edges <$> ((spaces >> P.string "edges" >> spaces >> equals) *> number)
            readMaxdegree = MaxDegree <$> ((spaces >> P.string "maxdegree" >> spaces >> equals) *> number)
            readMaxclique = MaxClique <$> ((spaces >> P.string "maxclique" >> spaces >> equals) *> number)
            readEdge = Edge <$> (spaces >> P.string "edge" >> spaces >> openPar *> readP) <*> (comma *> readP <* closePar)
            readDegree = Degree <$> (spaces >> P.string "degree" >> spaces >> openPar *> readP <* closePar) <*> (equals *> number)
            readNot = Not <$> (spaces >> P.string "Not" >> spaces >> openPar *> readP <* closePar)

instance (DAO (G.GraphConstraint Char)) GraphConstraint where
    toValue (Vertices a) = G.Vertices a
    toValue (Edges a) = G.Edges a
    toValue (MaxDegree a) = G.MaxDegree a
    toValue (MaxClique a) = G.MaxClique a
    toValue (Edge a b) = G.Edge (fromId a) (fromId b)
    toValue (Degree a b) = G.Degree (fromId a) b
    toValue (Not c) = G.Not (toValue c)