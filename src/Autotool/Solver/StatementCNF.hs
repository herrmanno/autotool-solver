module Autotool.Solver.StatementCNF (solve) where

import Prelude hiding ((&&), (||))
import qualified Data.Map as M
import Data.Tree (Tree(Node))
import Autotool.Data.StatementLogic
      ( Statement(..)
      , StatementOp
      , Interpretation
      , true
      , false
      , var
      , (!)
      , (&&)
      , (||)
      , Interpretable(..)
      , MaxTerm
      , maxterms
      , reduce
      )

import Autotool.DAO (toValue)
import qualified Autotool.DAO.Statement as DAO

solve :: Statement -> Statement
solve = toCNF

toCNF :: Statement -> Statement
toCNF s = case maxterms s of
    (Left b)     -> Statement $ Node (if b then true else false) []
    (Right maxs) -> let disjunctions = map toDisjunction (reduce maxs)
                        foldNodes a b = Node (&&) [a, b]
                    in Statement $ foldr1 foldNodes disjunctions

toDisjunction :: MaxTerm -> Tree StatementOp
toDisjunction is = foldr1 foldNodes $ map toNode (M.toList `onInterpretable` is)
      where
            foldNodes a b = Node (||) [a, b]
            toNode (c, False) = Node (var c) []
            toNode (c, True) = Node (!) [ Node (var c) [] ]
