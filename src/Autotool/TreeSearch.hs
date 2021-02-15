module Autotool.TreeSearch
    ( SearchMode(..)
    , evalModeDescription
    , searchTree
    , searchTreeUnevaluated
    , Tree
    , Op
    ) where

import Autotool.Data.LazyTree (Op, Tree)
import qualified Autotool.Data.LazyTree as ST
import qualified Autotool.Data.Parallel.LazyTree as PT

data SearchMode = Unlimited | Serial Int | Parallel Int deriving (Show, Read)

evalModeDescription :: String
evalModeDescription = unlines
    [ "The search mode. Available modes are"
    , "- Unlimited. Tries every possible term / expression (in serial)."
    , "  Use this only if parallel execution is no option and"
    , "  you really need a result. May burst your device into"
    , "  flames; use carefully."
    , "- Serial n, n > 0"
    , "  Evaluates n possible terms / expressions in serial, where"
    , "  the order of given operators defines how the terms are built."
    , "  Use this if your tasks asks for a large (>4) number of"
    , "  operators and try to re-order the operators manually to find a result."
    , "- Parallel n, n > 0"
    , "  Evaluates !m * n trees / expressions in parallel, where m is"
    , "  the number of given operators with artiy 1 or greater."
    , "  Use this if your number of operators is small (e.g. < 5) or your"
    , "  number of available CPU cores is tremendous."
    , "  Note that all !m forests must be computed before a result may"
    , "  be returned, so in situations where the task description asks for"
    , "  a large number of operators this mode is slower than the optimal"
    , "  serial evaluation (meaning given optimal operator order) but almost"
    , "  always faster then the worst serial evaluation."
    ]

searchTree :: (Show a, Eq a)
    => SearchMode               -- ^ the search mode
    -> [Op c a]                 -- ^ the operations defining the tree type
    -> c                        -- ^ the context to evaluate the tree in
    -> (a -> Bool)              -- ^ the predicate to test against
    -> Maybe (Tree (Op c a))    -- ^ a tree that satifies the predicate
searchTree Unlimited = \ops c p -> Just $ ST.searchTree ops c p
searchTree (Serial lim) = ST.searchTreeLim lim
searchTree (Parallel lim) = PT.searchTreeLimP lim

searchTreeUnevaluated :: (Show a, Eq a)
    => SearchMode               -- ^ the max number of trees to test
    -> [Op c a]                 -- ^ the operations defining the tree type
    -> (Tree (Op c a) -> Bool)  -- ^ the predicate to test against
    -> Maybe (Tree (Op c a))    -- ^ a tree that satifies the predicate
searchTreeUnevaluated Unlimited = \ops p -> Just $ ST.searchTreeUnevaluated ops p
searchTreeUnevaluated (Serial lim) = ST.searchTreeUnevaluatedLim lim
searchTreeUnevaluated (Parallel lim) = PT.searchTreeUnevaluatedLimP lim