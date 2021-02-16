module Autotool.Solver.StatementTransformSpec (spec) where

import Prelude hiding ((&&), (||))
import Test.Hspec ( describe, it, shouldBe )
import Autotool.Data.StatementLogic
    ( Statement(..)
    , equiv
    , var
    , true
    , false
    , (&&)
    , (||)
    , (!)
    , (-->)
    , (<-->)
    )
import Autotool.Solver.StatementTransform (solve)
import Data.Tree (Tree(Node))

spec = do
    describe "transform statement" $ do
        it "finds an equivalent statement for all basic statement in all junctor base [!, &&]" $
            let ops = [(!), (&&)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True
        it "finds an equivalent statement for all basic statement in all junctor base [!, ||]" $
            let ops = [(!), (||)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True
        it "finds an equivalent statement for all basic statement in all junctor base [!, ->]" $
            let ops = [(!), (-->)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True
        it "finds an equivalent statement for all basic statement in all junctor base [false, ->]" $
            let ops = [false, (-->)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True
        it "finds an equivalent statement for all basic statement in all junctor base [false, &&, <->]" $
            let ops = [false, (&&), (<-->)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True
        it "finds an equivalent statement for all basic statement in all junctor base [false, ||, <->]" $
            let ops = [false, (||), (<-->)]
                stms' = map (`solve` ops) stms
                r = zipWith equiv stms stms'
            in r `shouldBe` replicate (length stms) True

stms = map Statement
    [ Node a []
    , Node true []
    , Node false []
    , Node (!) [ Node a [] ]
    , Node (||) [ Node a [], Node b [] ]
    , Node (&&) [ Node a [], Node b [] ]
    , Node (-->) [ Node a [], Node b [] ]
    , Node (<-->) [ Node a [], Node b [] ]
    ]
    where [a,b] = [var 'a', var 'b']