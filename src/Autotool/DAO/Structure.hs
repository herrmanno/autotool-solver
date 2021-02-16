{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Autotool.DAO.Structure ( Struktur(..) ) where

import qualified Data.Map as M
import Autotool.Data.LazyTree (Op, mkOp0, mkOp1, mkOp2)
import Autotool.DAO (DAO(..))
import Autotool.DAO.Set (Set)
import Autotool.DAO.Map (Map)
import Autotool.DAO.Function (Function(..), maplet)
import Autotool.DAO.Identifier (Identifier, fromId)

data Struktur = Struktur
    { universum :: Set Int
    , predicates :: Map Identifier (Function Int)
    , functions :: Map Identifier (Function Int)
    } deriving (Show,Read)

instance (DAO [Op c Int]) Struktur where
    toValue s = ops
        where
            ops = map toOp $ M.toList (toValue $ functions s)
            toOp (i, f@(Fn0 _)) = mkOp0 [fromId i] (maplet f [])
            toOp (i, f@(Fn1 _)) = mkOp1 [fromId i] (maplet f . (: []))
            toOp (i, f@(Fn2 _)) = mkOp2 [fromId i] False (\ x y -> maplet f [x,y])