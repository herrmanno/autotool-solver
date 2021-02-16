{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Autotool.DAO (DAO(..)) where


import Data.Set (Set)
import qualified Data.Set as S (map)
class DAO a b where
    -- | Unwraps a data access object
    toValue :: b -> a

instance {-# OVERLAPPABLE #-} (DAO a) a where
    toValue = id

instance {-# OVERLAPPABLE #-} ((DAO b) a) => (DAO [b]) [a] where
    toValue = map toValue

instance {-# OVERLAPPABLE #-} (Ord a, Ord b, (DAO b) a) => (DAO (Set b)) (Set a) where
    toValue = S.map toValue