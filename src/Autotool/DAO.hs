{-# LANGUAGE MultiParamTypeClasses #-}

module Autotool.DAO (DAO(..)) where

class DAO a b where
    -- | Unwraps a data access object
    toValue :: b -> a