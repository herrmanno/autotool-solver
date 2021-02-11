module Autotool.Solver.GraphParamSpec (spec) where

import Test.Hspec
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.Graph as DAO
import Autotool.Data.Graph (Graph)
import Autotool.Solver.GraphParam (solve)

spec = do
    describe "graph param" $ do
        it "finds a graph satisfying a set of constraints" $
            let u = read "[a,b,c,d,e,f]" :: [DAO.Identifier]
                cs = read "[ vertices = 6 , edges = 10 , maxdegree = 4 , maxclique = 4 , edge ( f, a) , edge ( e, a) , edge ( e, f) , Not (edge ( b, c)) , Not (edge ( d, e)) , degree (b) = 3 , degree (c) = 3 , degree (e) = 4 , degree (d) = 2 , degree (f) = 4 ]" :: [DAO.GraphConstraint]
                (Just g) = solve (toValue cs) (toValue u) 
                r = read "Graph {knoten = mkSet [a,b,c,d,e,f], kanten = mkSet [kante a b,kante a c,kante b d,kante b e,kante c e,kante c f,kante d f,kante e a,kante e f,kante f a]}" :: DAO.Graph DAO.Identifier
            in (g :: Graph Char) `shouldBe` toValue r
