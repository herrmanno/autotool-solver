module Autotool.Data.Set (S(..), insert, union, diff, intersect, compose, pow) where

import Data.List (nub, intercalate, concatMap, )
import Data.Function (on)

data S a = V a | S [S a]  | Set String [S a]

instance (Show a) => Show (S a) where
    show (V a) = show a
    show (Set n _) = n
    show (S []) = "{}"
    show (S a) = "{" ++ intercalate ", " (map show a) ++ "}"

instance (Eq a) => Eq (S a) where
    (V a) == (V b) = a == b
    (S a) == (S b) = all (`elem` b) a && all (`elem` a ) b
    (S a) == (Set _ b) = all (`elem` b) a && all (`elem` a ) b
    (Set _ a) == (S b) = S a == S b
    (Set _ a) == (Set _ b) = S a == S b
    _ == _ = False

smap :: (S a -> S b) -> S a -> S b
smap f (S xs) = S $ fmap f xs

toS :: S a -> S a
toS s@(S xs) = s
toS (Set _ xs) = S xs
toS (V _) = undefined

insert :: (Eq a) => S a -> S a -> S a
insert v@(V a) _ = v
insert s@(Set _ xs) b = insert (toS s) b
insert (S xs) b
    | b `elem` xs = S xs
    | otherwise = S $ b:xs

union :: (Eq a, Show a) => S a -> S a -> S a
union (S xs) (S ys) = S $ nub (xs ++ ys)
union a b = (union `on` toS) a b

diff :: (Eq a) => S a -> S a -> S a
diff (S a) (S b) = S $ filter (`notElem` b) a
diff a b = (diff `on` toS) a b

intersect :: (Eq a) => S a -> S a -> S a
intersect (S a) (S b) = S $ filter (`elem` b) a
intersect a b = (intersect `on` toS) a b

compose :: (Eq a) => S (a,a) -> S (a,a) -> S (a,a)
compose (S xs) (S ys) = S $ concatMap f xs where
    f (V (a,b)) = map (\(V (c,d)) -> V (a,d)) $ filter (\(V (c,d)) -> b == c) ys
compose a b = (compose `on` toS) a b

pow :: (Eq a, Show a) => S a -> S a
pow (V a) = V a
pow (S []) = S[S[]]
pow (S (x:xs)) =
    let pxs = pow $ S xs
        a = smap (`insert` x) pxs
        b = pxs
    in union a b
pow a = pow $ toS a