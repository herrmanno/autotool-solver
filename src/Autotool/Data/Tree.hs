module Autotool.Data.Tree (Tree(..), size, buildTrees) where

import Control.Monad (guard)
import Autotool.Data.Set (S)
import Autotool.Data.Op(Op2(..), Op1(..))
import Autotool.Util.Hash (hnub)

data Tree a = Node2 Op2 (Tree a) (Tree a) | Node1 Op1 (Tree a) | Node0 (S a)

instance (Eq a) => Eq (Tree a) where
    Node0 a == Node0 b = a == b
    Node1 _ a == Node1 _ b = a == b
    Node2 Subtr a b == Node2 Subtr a' b' = a == a' && b == b'
    Node2 op a b == Node2 op' a' b' = op == op' && ((a == a' && b == b') || (a == b' && b' == a))
    _ == _ = False

instance (Show a) => Show (Tree a) where
    show (Node0 a) = show a
    show (Node1 op a) = show op ++ "(" ++ show a ++ ")"
    show (Node2 op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"

size :: Tree a -> Int
size (Node0 _) = 1
size (Node1 _ a) = 1 + size a
size (Node2 _ a b) = 1 + size a + size b

buildTrees :: (Eq a, Show a) => [Op2] -> [Op1] -> [S a] -> Int -> [Tree a]
buildTrees _ _ ss 0 = hnub $ map Node0 ss
buildTrees op2s op1s ss d = hnub $ do
    l <- buildTrees op2s op1s ss (d-1)
    r <- buildTrees op2s op1s ss (d-1)
    guard $ l /= r
    let op2Trees = map (\op -> Node2 op l r ) op2s
        op1Trees = map (`Node1` l ) op1s
        op0Trees = map Node0 ss
    op2Trees ++ op1Trees ++ op0Trees