{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Autotool.Result where

import Control.Applicative (Alternative(..))
import Data.Kind (Type)

type family RT t a where
    RT 'Empty a = a
    RT 'Present a = a

data ResultType a = Empty a | Present a

data TaskResult a where
    Error :: String -> TaskResult (RT Empty a)
    Result :: a -> TaskResult (RT Present a)
    -- Results :: [a] -> TaskResult (Present a)
    -- Warnings :: [String] -> TaskResult (Present a) -> TaskResult (Present a)

e :: TaskResult String
e = Error ""
r :: TaskResult [Char]
r = Result ""

-- foo :: TaskResult a -> TaskResult b
-- foo (Result r) = Result ""

-- deriving instance (Show a) => Show (TaskResult (ResultType a))

-- data AnyResult a where
--     AnyResult :: TaskResult 'Present a -> AnyResult a
--     AnyError :: TaskResult t String -> AnyResult a

-- instance Functor AnyResult where
--     fmap f (AnyResult r) = AnyResult $ fmap f r
--     fmap _ (AnyError te) = AnyError te

-- instance Functor TaskResult where
--     fmap f (Error e) = Error e
--     fmap f (Result r) = Result (f r)
--     fmap f (Results rs) = Results (fmap f rs)

-- instance Applicative (TaskResult t) where
--     _ <*> (Error _) = None
--     -- TODO: impl
--     pure = const None

-- instance (t~'Present) => Applicative (TaskResult t) where
--     (<*>) = ap
--     -- (Result f) <*> (Result r) = Result (f r)
--     -- TODO: results
--     pure = Result

-- instance Alternative (TaskResult t) where
--     (Error e) <|> b = b
--     a <|> (Error e) = a
--     None <|> b = b
--     a <|> None = a
--     a <|> _ = a
--     empty = None

-- instance Monad (TaskResult t) where
--     None >>= _ = None
--     (Error e) >>= _ = Error e
--     (Result r) >>= f = f r
--     (Results rs) >>= f = mapM f rs >>= Results
--     return = pure

-- instance (t~'Empty) => MonadFail (TaskResult t) where
--     fail = Error

-- fromEither :: Either String a -> TaskResult t a
-- fromEither (Left e) = Error e
-- fromEither (Right a) = Result a

-- r :: TaskResult 'Present Int
-- r = Result 1
-- w :: TaskResult 'Present Int
-- w = Warnings ["bar"] r
-- e :: TaskResult 'Empty String
-- e = Error "foo"

-- results :: [AnyResult Int]
-- results = [AnyResult r, AnyResult w, AnyError e]
-- rs' = map f results
--     where
--         f ar = Just <$> ar
        
-- consume :: TaskResult t a -> Maybe a
-- consume (Result r) = Just r
-- consume _ = Nothing