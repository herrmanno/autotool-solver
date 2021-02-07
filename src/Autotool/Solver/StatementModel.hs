module Autotool.Solver.StatementModel (solve) where

-- import Data.Set (delete, insert,  empty, toList )
import Autotool.Data.StatementLogic ( Statement(tree), Interpretation, universe, interpretations )
import Data.Foldable (find)
import Autotool.Data.LazyTree (evalTree')


solve :: Statement -> Interpretation
solve s = case find (\c -> evalTree' c (tree s) ) is of
            (Just i) -> i
            _ -> error "Could not find a model"
    where is = interpretations $ universe s 

