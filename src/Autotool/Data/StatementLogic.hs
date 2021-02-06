{-# LANGUAGE AllowAmbiguousTypes #-}
module Autotool.Data.StatementLogic
    ( Statement(..)
    , VarID
    , Universe
    , Interpretation
    , StatementOp
    , universe
    , interpretations
    , model
    , equiv
    , true
    , false
    , var
    , (&&)
    , (||)
    , (!)
    , (-->)
    , (<-->)
    ) where

import Prelude hiding ((&&), (||))
import qualified Prelude as Pre ((&&), (||))
import qualified Data.Map as M
import Data.Tree ( Tree(Node), foldTree )
import Autotool.Data.LazyTree (evalTree', isOp0, Op, mkOp0, mkOp0C, mkOp1, mkOp2)
import Data.List (find, nub)
import Data.Function (on)

type VarID = Char

type Universe = [VarID]

type Interpretation = M.Map VarID Bool

type StatementOp = Op Interpretation Bool

newtype Statement = Statement { tree :: Tree StatementOp } deriving (Eq, Show)

universe :: Statement -> Universe
universe = map head . foldTree f . tree
    where
        f leaf children
            | isVar leaf = [show leaf]
            | otherwise = nub $ concat children
        isVar l = isOp0 l Pre.&& (1 == length (show l)) -- TODO: find a better way to identiy leafs

interpretations :: Universe -> [Interpretation]
interpretations [] = [M.singleton '_' False, M.singleton '_' True]
interpretations [x] = [M.singleton x False, M.singleton x True]
interpretations (x:xs) = do
    i <- interpretations xs
    [M.insert x False i, M.insert x True i]

equiv :: Statement -> Statement -> Bool
equiv = (==) `on` model

model :: Statement -> [(Interpretation, Bool)]
model s = let t = tree s
              is = interpretations $ universe s
          in zip is $ map (`evalTree'` t) is

-- operations

true :: Op Interpretation Bool
true = mkOp0 "true" True

false :: Op Interpretation Bool
false = mkOp0 "false" False

var :: VarID -> Op Interpretation Bool
var c = mkOp0C [c] (M.! c)

(!) :: Op Interpretation Bool
(!) = mkOp1 "!" not

(&&) :: Op Interpretation Bool
(&&) = mkOp2 "&&" True (Pre.&&)

(||) :: Op Interpretation Bool
(||) = mkOp2 "||" True (Pre.||)

(-->) :: Op Interpretation Bool
(-->) = mkOp2 "->" True implication
    where implication a b = not a Pre.|| b

(<-->) :: Op Interpretation Bool
(<-->) = mkOp2 "<->" True equivalence
    where equivalence a b = a == b
