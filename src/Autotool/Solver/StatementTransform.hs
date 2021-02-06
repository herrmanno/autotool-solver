module Autotool.Solver.StatementTransform (solve, JunctorBase) where

import Prelude hiding ((&&), (||))
import Data.Tree (foldTree, Tree(..))
import Data.Foldable (find)
import Autotool.Data.StatementLogic
      (universe,  Statement(..)
      , StatementOp
      , Universe
      , true
      , false
      , var
      , (&&)
      , (||)
      , (!)
      , (-->)
      , (<-->)
      )
import Autotool.Data.LazyTree (showTree, evalTree')
import Autotool.Data.Parallel.LazyTree (searchTreeUnevaluatedLimP)


solve ::
      Statement
      -> [StatementOp]
      -> Statement
solve s ops = case jb of
      Just j -> transformStatement j s
      _ -> error (
            "Cannot find a junctor base for operator combination " ++ show ops ++ ". " ++
            "Available junctor bases are: " ++ show (map operatorsJB (enumFrom minBound)))
      where jb = pickJB ops


data JunctorBase = NegAnd
                 | NegOr
                 | NegImpl
                 | FalseImpl
                 | FalseAndEquiv
                 | FalseOrEquiv
                 deriving (Eq,Enum,Bounded,Show)

pickJB :: [StatementOp] -> Maybe JunctorBase
pickJB ops = find f (enumFrom minBound)
    where f jb = all (`elem` ops) (operatorsJB jb)

operatorsJB :: JunctorBase -> [StatementOp]
operatorsJB NegAnd = [(!), (&&)]
operatorsJB NegOr = [(!), (||)]
operatorsJB NegImpl = [(!), (-->)]
operatorsJB FalseImpl = [false, (-->)]
operatorsJB  FalseAndEquiv = [false, (&&), (<-->)]
operatorsJB  FalseOrEquiv = [false, (||), (<-->)]

transformStatement :: JunctorBase -> Statement -> Statement
transformStatement jb s = Statement $ foldTree f (tree s)
    where
        f = transformOp jb u
        u = universe s

transformOp :: JunctorBase -> Universe -> StatementOp -> [Tree StatementOp] -> Tree StatementOp
--  Juncor Base: &&, !
transformOp jb@NegAnd u j [a,b] | j == (||) = Node (!) [ Node(&&) [ Node (!) [a], Node (!) [b]  ] ]
transformOp jb@NegAnd u j [a,b] | j == (-->) = transformOp jb u (||) [ Node (!) [a], b ]
transformOp jb@NegAnd u j [a,b] | j == (<-->) = Node (&&) [ transformOp jb u (-->) [a,b], transformOp jb u (-->) [b,a] ]
transformOp jb@NegAnd u@(x:_) j  _ | j == true = transformOp jb u (<-->) [ Node (var x) [], Node (var x) [] ]
transformOp jb@NegAnd u@(x:_) j  _ | j == false = Node (&&) [ Node (var x) [], Node (!) [ Node (var x) [] ] ]
transformOp jb@NegAnd _ j c = Node j c
--  Juncor Base: ||, !
transformOp jb@NegOr u j [a,b] | j == (&&) = Node (!) [ Node(||) [ Node (!) [a], Node (!) [b]  ] ]
transformOp jb@NegOr u j [a,b] | j == (-->) = Node (||) [ Node (!) [a], b ]
transformOp jb@NegOr u j [a,b] | j == (<-->) = transformOp jb u (&&) [ transformOp jb u (-->) [a,b], transformOp jb u (-->) [b,a] ]
transformOp jb@NegOr u@(x:_) j _ | j == true = Node (||) [ Node (var x) [], Node (!) [ Node (var x) [] ] ]
transformOp jb@NegOr u@(x:_) j _ | j == false = transformOp jb u (&&) [ Node (var x) [], Node (!) [ Node (var x) [] ] ]
transformOp jb@NegOr _ j c = Node j c
--  Juncor Base: ->, !
transformOp jb@NegImpl u j [a,b] | j == (||) = Node (-->) [ Node (!) [a], b ]
transformOp jb@NegImpl u j [a,b] | j == (&&) = Node (!) [ transformOp jb u (||) [ Node (!) [a], Node (!) [b]  ] ]
transformOp jb@NegImpl u j [a,b] | j == (<-->) = transformOp jb u (&&) [ Node (-->) [a,b], Node (-->) [b,a] ]
transformOp jb@NegImpl u@(x:_) j _ | j == true = transformOp jb u (||) [ Node (var x) [], Node (!) [ Node (var x) [] ] ]
transformOp jb@NegImpl u@(x:_) j _ | j == false = transformOp jb u (&&) [ Node (var x) [], Node (!) [ Node (var x) [] ] ]
transformOp jb@NegImpl _ j c = Node j c
--  Juncor Base: false, ->
transformOp jb@FalseImpl u j [a] | j == (!) = Node (-->) [ a, Node false [] ]
transformOp jb@FalseImpl u j [a,b] | j == (||) = Node (-->) [ transformOp jb u (!) [a], b ]
transformOp jb@FalseImpl u j [a,b] | j == (&&) = transformOp jb u (!) [ transformOp jb u (||) [ transformOp jb u (!) [a], transformOp jb u (!) [b]  ] ] -- TODO: improve
transformOp jb@FalseImpl u j [a,b] | j == (<-->) = transformOp jb u (&&) [ Node (-->) [a,b], Node (-->) [b,a] ]
transformOp jb@FalseImpl u@(x:_) j _ | j == true = Node (-->) [ Node false [], Node false [] ]
transformOp jb@FalseImpl _ j c = Node j c
-- Junctor Base: false, &&, <->
transformOp jb@FalseAndEquiv u j [a] | j == (!) = Node (<-->) [ a, Node false [] ]
transformOp jb@FalseAndEquiv u j [a,b] | j == (||) = transformOp jb u (!) [ Node(&&) [ transformOp jb u (!) [a], transformOp jb u (!) [b]  ] ] -- TODO: improve
transformOp jb@FalseAndEquiv u j [a,b] | j == (-->) = transformOp jb u (||) [ transformOp jb u (!) [a], b ]
transformOp jb@FalseAndEquiv u@(x:_) j _ | j == true = transformOp jb u (!) [ Node false [] ]
transformOp jb@FalseAndEquiv _ j c = Node j c
-- Junctor Base: false, ||, <->
transformOp jb@FalseOrEquiv u j [a] | j == (!) = Node (<-->) [ a, Node false [] ]
transformOp jb@FalseOrEquiv u j [a,b] | j == (&&) = transformOp jb u (!) [ Node(||) [ transformOp jb u (!) [a], transformOp jb u (!) [b]  ] ] -- TODO: improve
transformOp jb@FalseOrEquiv u j [a,b] | j == (-->) = Node (||) [ transformOp jb u (!) [a], b ]
transformOp jb@FalseOrEquiv u@(x:_) j _ | j == true = transformOp jb u (!) [ Node false [] ]
transformOp jb@FalseOrEquiv _ j c = Node j c