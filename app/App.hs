module App ( app, repl, taskHelp, taskTypeDescriptions, replDescriptions ) where

import Task (Task(..), describeTask, shortDescribeTask, TaskInput, TaskResult(..))
import qualified Tasks.Sets as Set ( task )
import qualified Tasks.MultiSets as MultiSet ( task )
import qualified Tasks.Relations as Relation ( task )
import qualified Tasks.Structures as Structures ( task )
import qualified Tasks.Graphs as Graphs ( task )
import qualified Tasks.Circle as Circle ( task )
import qualified Tasks.Hamilton as Hamilton ( task )
import qualified Tasks.Bipartit as Bipartit ( task )
import qualified Tasks.Isomorphism as Isomorphism ( task )
import qualified Tasks.StatementModel as StatementModel ( task )
import qualified Tasks.StatementEquivalent as StatementEquivalent ( task )
import qualified Tasks.StatementTransform as StatementTransform ( task )
import qualified Tasks.StatementCNF as StatementCNF ( task )
import qualified Tasks.StatementDNF as StatementDNF ( task )
import qualified Tasks.GraphParam as GraphParam ( task )
import Repl (Repl(..), shortDescribeRepl)
import qualified REPL.Sets as Set ( repl )
import qualified REPL.MultiSets as MultiSet ( repl )
import qualified REPL.Relations as Relation ( repl )

import Data.Foldable (find)
import Data.Char (toLower)
import Data.Function (on)


tasks :: [Task]
tasks =
    [ StatementModel.task
    , StatementEquivalent.task
    , StatementTransform.task
    , StatementCNF.task
    , StatementDNF.task
    , Set.task
    , MultiSet.task
    , Relation.task
    , Graphs.task
    , Circle.task
    , Hamilton.task
    , Bipartit.task
    , Isomorphism.task
    , GraphParam.task
    , Structures.task
    ]

repls :: [Repl]
repls =
    [ Set.repl
    , MultiSet.repl
    , Relation.repl
    ]

app :: String -- ^ task type
    -> TaskInput -- ^ task description
    -> TaskResult String -- ^ output
app t d = case findTask t of
    (Just t) -> runTask t d
    _        -> Error $ "Task '" ++ t ++ "' not found."

repl :: String -> IO String
repl t = case findRepl t of
    Just r -> loop r >> return ""
    _      -> return $ "Repl '" ++ t  ++ "' not found."

taskHelp t = case findTask t of
    (Just t) -> describeTask t
    _        -> unknownTasktype t

taskTypeDescriptions :: [String]
taskTypeDescriptions = map shortDescribeTask tasks

replDescriptions :: [String]
replDescriptions = map shortDescribeRepl repls

findTask t = find (compare t . Task.name) tasks
    where compare = (==) `on` map toLower
    
findRepl t = find (compare t . Repl.name) repls
    where compare = (==) `on` map toLower

unknownTasktype t = "Unknown task type '" ++ t ++ "'. Available tasks are:\n" ++ taskTypes
    where taskTypes = unlines $ map (\t -> " - " ++ Task.name t) tasks
