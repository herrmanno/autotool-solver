module Autotool.Solver.StatementEquivalent (solve) where

import Autotool.Data.StatementLogic
      ( Statement(..)
      , StatementOp
      , universe
      , equiv
      , truthTable
      , var
      )
import Autotool.Data.LazyTree (showTree, evalTree')
import Autotool.Data.Parallel.LazyTree (searchTreeUnevaluatedLimP)
import Debug.Trace (traceShow, traceShowId)


solve ::
      Statement
      -> [StatementOp]
      -> Statement
solve s ops = case searchTreeUnevaluatedLimP lim (consts ++ ops) (equiv s . Statement) of
            (Just i) -> Statement i
            _ -> error "Could not find an equivalent statement"
    where
          lim = 300000
          u = universe s
          consts = map var u -- TODO: move to StatementLogic module

