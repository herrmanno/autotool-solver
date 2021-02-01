module Autotool.Data.NestedSet
    ( T
    , NSet
    , value
    , set
    , isValue
    , isSet
    , unwrapValue
    , unwrapSet
    , unwrap
    , toNSet
    , ø
    , (+.)
    , (++.)
    , (&.)
    , powerSet
    , toStr
    , toStrFn
) where

import Data.Set (Set, empty, fromList, toList, insert)
import qualified Data.Set as S
import Data.List (intercalate)

-- | Recursive sum type of 'either a value or a set of Ts'
data T a = V a | S (Set (T a)) deriving (Eq, Ord, Show)

-- | A Data.Set of type (T a)
type NSet a = Set (T a)

-- for internal (e. g. parser) use
value :: a -> T a
value = V

set :: Set (T a) -> T a
set = S

isValue :: T a -> Bool
isValue (V a)  = True
isValue _ = False

isSet :: T a -> Bool
isSet (S a)  = True
isSet _ = False

unwrapValue :: T a -> a
unwrapValue (V a) = a

unwrapSet :: T a -> S.Set (T a)
unwrapSet (S a) = a

unwrap :: T a -> NSet a
unwrap (S s) = s

ø :: Set a
ø = empty

øN :: (Ord a) => Int -> NSet a
øN = emptyN

emptyN :: (Ord a) => Int -> NSet a
emptyN 1 = empty
emptyN n = emptyN (n - 1) &. empty

(+.) :: (Ord a) => a -> NSet a -> NSet a
infixr 2 +.
a +. s = insert (V a) s

(++.) :: (Ord a) => [a] -> NSet a -> NSet a
infixr 2 ++.
a ++. s = insert (S $ fromList $ map V a) s

(&.) :: (Ord a) => NSet a -> NSet a -> NSet a
infixr 2 &.
a &. s = insert (S a) s
--

powerSet :: (Ord a) => NSet a -> NSet a
powerSet s = S.map S $ S.powerSet s

toNSet :: (Ord a) => Set a -> NSet a
toNSet = S.map V

toStr :: (Show a) => NSet a -> String
toStr = toStrFn show

toStrFn :: (Show a) => (a -> String) -> NSet a -> String
toStrFn showValue s = "{" ++ intercalate ", " (map toStr' (toList s)) ++ "}"
    where
        toStr' (V a) = showValue a
        toStr' (S a) = "{" ++ intercalate ", " (map toStr' (toList a)) ++ "}"