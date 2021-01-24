import Prelude hiding ((+), (-), (*))

import Autotool.Data.RelOp ( (&), (+), (-), (*) )
import Autotool.Data.LazyTree (showTreeFn, showTree,  Tree(Node), Op(Op0) )
import Autotool.Solver.Relations (solve)
import Autotool.Parser.Relation (parseIntRelation)
import Autotool.Data.Parallel.LazyTree (treesP)
main = do
        let
            r = parseIntRelation "{(1,1),(1,2),(2,1)}"
                -- s = Set "S" [ V(1 , 3), V(2 , 1), V(2 , 2) ]
            s = parseIntRelation "{(1,3),(2,1),(2,2)}"
                -- t = S[ V(1 , 2) , V(2 , 3) ]
            t = parseIntRelation "{(1,3),(2,3)}"
            ops = [(+), (&), (-), (*), Op0 r, Op0 s]
                -- result = Node2 Subtr (Node2 Compose (Node0 r) (Node2 Subtr (Node0 s) (Node0 r))) (Node0 s)
            result = Node (-) [
                Node (*) [
                    Node (Op0 r) [],
                    Node (-) [ Node (Op0 s) [], Node (Op0 r) [] ]
                ],
                Node (Op0 s) []
                ]
            ts = treesP ops
            st = let f a
                        | a == r = "R"
                        | a == s = "S"
                        | otherwise = show a
                in showTreeFn f
        -- mapM_ (putStrLn . st) ts
        print $ solve ops t

