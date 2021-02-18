module Repl (Repl(..), shortDescribeRepl) where

data Repl = Repl
    { name :: String
    , description :: String
    , loop :: IO ()
    }

shortDescribeRepl :: Repl -> String
shortDescribeRepl t = unwords
    [ name t ++ replicate (10 - length (name t)) ' '
    , ":: "
    , description t
    ]