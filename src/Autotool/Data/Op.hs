module Autotool.Data.Op (Op2(..), Op1(..)) where

data Op2 =
      Add       -- ^ union of two objects (e.g. sets, relations, graphs)
    | Subtr     -- ^ difference of two objects (e.g. sets, relations, graphs)
    | And       -- ^ intersection of two objects (e.g. sets, relations, graphs)
    | Compose   -- ^ composition of two relations
    | Junction  -- ^ Junection (*) of two graphs
    deriving (Eq, Enum)

data Op1 =
      Pow        -- ^ powersets of a set
    | Complement -- ^ complement of an graph
    deriving (Eq, Enum)

instance Show Op2 where
    show Add = "+"
    show Subtr = "-"
    show And = "&"
    show Compose = "."
    show Junction = "*"

instance Show Op1 where
    show Pow = "pow"
    show Complement = "co"
