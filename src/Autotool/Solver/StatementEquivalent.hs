module Autotool.Solver.StatementEquivalent (solve) where

import Autotool.Data.StatementLogic
      ( Statement(..)
      , StatementOp
      , universe
      , equiv
      , var
      )
import Autotool.TreeSearch (SearchMode, searchTreeUnevaluated)


solve :: SearchMode           -- ^ the search mode
      -> Statement            -- ^ the original statement
      -> [StatementOp]        -- ^ operators allowed in the final statement
      -> Statement            -- ^ an equivalent statement
solve m s ops = case searchTreeUnevaluated m (consts ++ ops) (equiv s . Statement) of
            (Just i) -> Statement i
            _ -> error "Could not find an equivalent statement"
    where
          u = universe s
          consts = map var u -- TODO: move to StatementLogic module

