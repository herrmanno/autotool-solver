module App ( app, help ) where

import Tasks.Sets as Set ( runTask ) 
import Tasks.Relations as Relation ( runTask )
import Tasks.Structures as Structures ( runTask )
import Tasks.Graphs as Graphs ( runTask )
import Tasks.Circle as Circle ( runTask )
import Tasks.Hamilton as Hamilton ( runTask )
import Tasks.Bipartit as Bipartit ( runTask )

app :: String -- ^ task type
    -> String -- ^ task description
    -> String -- ^ output
app "set" d = Set.runTask d
app "rel" d = Relation.runTask d
app "struct" d = Structures.runTask d
app "graph" d = Graphs.runTask d
app "circle" d = Circle.runTask d
app "hamilton" d = Hamilton.runTask d
app "bipartit" d = Bipartit.runTask d
app s _ = unlines $ ("Undefined task type '" ++ s ++ "'. Supported task types are:") : taskDescriptions

help = unlines $ "USAGE: <task> <task description file>" : taskDescriptions

taskDescriptions =
    [ "  - set :: finds an expression matching a target value, given operators and constants"
    , "  - rel :: finds an expression matching a target value, given operators and constants"
    , "  - struct :: finds an expression with different value (semantic) in two structs"
    , "  - graph :: finds an expression matching a target value, given operators and constants"
    , "  - circle :: finds a circle of given length in a graph"
    , "  - hamilton :: finds a hamilton path in a graph"
    , "  - bipartit :: finds a set of vertices that split a graph into two bipartit subgraphs"
    ]