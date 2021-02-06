module Autotool.Solver.StatementEquivalent (solve) where

import Autotool.Data.StatementLogic
      ( Statement(..)
      , StatementOp
      , Interpretation
      , universe
      , interpretations
      , model
      , var
      )
import Autotool.Data.LazyTree (showTree, evalTree')
import Autotool.Data.Parallel.LazyTree (searchTreeUnevaluatedLimP)
import Debug.Trace (traceShow, traceShowId)


solve ::
      Statement
      -> [StatementOp]
      -> Statement
solve s ops = case searchTreeUnevaluatedLimP lim (consts ++ ops) p of
            (Just i) -> Statement i
            _ -> error "Could not find an equivalent statement"
    where
          lim = 300000
          u = universe s
          consts = map var u
          mod = model s
          p = (==mod) . model . Statement

