module Autotool.Solver.StatementDNF (solve) where

import Prelude hiding ((&&), (||))
import qualified Data.Map as M
import Data.Tree (Tree(Node))
import Autotool.Data.StatementLogic
      ( Statement(..)
      , StatementOp
      , true
      , false
      , var
      , (!)
      , (&&)
      , (||)
      , Interpretable(..)
      , MinTerm
      , minterms
      , reduce
      )


solve :: Statement -> Statement
solve = toDNF

toDNF :: Statement -> Statement
toDNF s = case minterms s of
    (Left b)     -> Statement $ Node (if b then true else false) []
    (Right mins) -> let conjunctions = map toConjunction (reduce mins)
                        foldNodes a b = Node (||) [a, b]
                    in Statement $ foldr1 foldNodes conjunctions

toConjunction :: MinTerm -> Tree StatementOp
toConjunction is = foldr1 foldNodes $ map toNode (M.toList `onInterpretable` is)
      where
            foldNodes a b = Node (&&) [a, b]
            toNode (c, True) = Node (var c) []
            toNode (c, False) = Node (!) [ Node (var c) [] ]
