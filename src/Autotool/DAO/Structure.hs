{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Autotool.DAO.Structure ( Struktur(..) ) where

import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Autotool.Data.LazyTree (Op(..))
import Autotool.DAO (DAO(..))
import Autotool.Readable (Readable(..))
import Autotool.DAO.Set (Set)
import Autotool.DAO.Map (Map)
import Autotool.DAO.Function (Function(..), maplet)
import Autotool.DAO.Identifier (Identifier(..))

data Struktur = Struktur
    { universum :: Set Int
    , predicates :: Map Identifier (Function Int)
    , functions :: Map Identifier (Function Int)
    } deriving (Show,Read)

instance (DAO [Op Int]) Struktur where
    toValue s = ops
        where
            ops = map toOp $ M.toList (toValue $ functions s)
            toOp (Id a, f@(Fn0 _)) = Op0 [a] (maplet f [])
            toOp (Id a, f@(Fn1 _)) = Op1 [a] (maplet f . (: []))
            toOp (Id a, f@(Fn2 _)) = Op2 [a] False (\ x y -> maplet f [x,y])