module App ( app, help ) where

import Tasks.Sets as Set ( runTask ) 
import Tasks.Relations as Relation ( runTask )
import Tasks.Structures as Structures ( runTask )
import Tasks.Graphs as Graphs ( runTask )
import Tasks.Circle as Circle ( runTask )
import Tasks.Hamilton as Hamilton ( runTask )
import Tasks.Bipartit as Bipartit ( runTask )
import Tasks.Isomorphism as Isomorphism ( runTask )
import Tasks.StatementModel as StatementModel ( runTask )
import Tasks.StatementEquivalent as StatementEquivalent ( runTask )
import Tasks.StatementTransform as StatementTransform ( runTask )
import Tasks.StatementCNF as StatementCNF ( runTask )
import Tasks.StatementDNF as StatementDNF ( runTask )

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
app "iso" d = Isomorphism.runTask d
app "al-model" d = StatementModel.runTask d
app "al-equiv" d = StatementEquivalent.runTask d
app "al-trans" d = StatementTransform.runTask d
app "al-cnf" d = StatementCNF.runTask d
app "al-dnf" d = StatementDNF.runTask d
app s _ = unlines $ ("Undefined task type '" ++ s ++ "'. Supported task types are:") : taskDescriptions

help :: String
help = unlines $ "USAGE: <task> <task description file>" : taskDescriptions

taskDescriptions =
    [ "  - set      :: finds an expression matching a target value, given operators and constants"
    , "  - rel      :: finds an expression matching a target value, given operators and constants"
    , "  - struct   :: finds an expression with different value (semantic) in two structs"
    , "  - graph    :: finds an expression matching a target value, given operators and constants"
    , "  - circle   :: finds a circle of given length in a graph"
    , "  - hamilton :: finds a hamilton path in a graph"
    , "  - bipartit :: finds a set of vertices that split a graph into two bipartit subgraphs"
    , "  - iso      :: finds an isomorphism from one graph to another"
    , "  - al-model :: finds a model for a statement of propositional logic"
    , "  - al-equiv :: finds an equivalent statement by brute forcing"
    , "  - al-trans :: finds an equivalent statement by fixed transformation rules"
    , "  - al-cnf :: finds a semantically equivalent cnf for a given statement"
    , "  - al-dnf :: finds a semantically equivalent dnf for a given statement"
    ]