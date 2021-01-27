module App ( app, help ) where

import Tasks.Sets as Set 
import Tasks.Relations as Relation
import Tasks.Structures as Structures

app :: String -- ^ task type
    -> String -- ^ task description
    -> String -- ^ output
app "set" d = Set.runTask d
app "rel" d = Relation.runTask d
app "struct" d = Structures.runTask d
app s _ = unlines $ ("Undefined task type '" ++ s ++ "'. Supported task types are:") : taskDescriptions

help = unlines $ "USAGE: <task> <task description file>" : taskDescriptions

taskDescriptions =
    [ "  - set :: finds an expression matching a target value, given operators and constants"
    , "  - rel :: finds an expression matching a target value, given operators and constants"
    ]