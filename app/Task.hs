module Task
    ( Task(..)
    , TaskInput(..)
    -- , TaskOutput(..)
    , TaskResult(..)
    , fromEither
    , readInputM
    , defaultTaskInput
    , readInput
    -- , defaultTaskOutput
    -- , fromResult
    -- , fromResults
    -- , fromEither
    , describeTask
    , shortDescribeTask
    ) where

import Text.Read (readEither)
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple ( pShow, pShowOpt, defaultOutputOptionsNoColor, OutputOptions(..), pStringOpt )
import Control.Applicative (Alternative(..))
import Control.Monad (join)

data TaskResult a = Error String | Result a  | Results [a] --  | WithWarnings [String] (TaskResult a)

instance Functor TaskResult where
    fmap f (Error e) = Error e
    fmap f (Result r) = Result (f r)
    fmap f (Results rs) = Results (fmap f rs)

instance Applicative TaskResult where
    f <*> (Error e) = Error e
    (Result f) <*> (Result r) = Result (f r)
    pure = Result

instance Alternative TaskResult where
    (Error e) <|> b = b
    a <|> (Error e) = a
    a <|> _ = a
    empty = Error ""

instance Monad TaskResult where
    (Error e) >>= _ = Error e
    (Result r) >>= f = f r
    (Results rs) >>= f = mapM f rs >>= Results
    return = pure

instance MonadFail TaskResult where
    fail = Error

fromEither :: Either String a -> TaskResult a
fromEither (Left e) = Error e
fromEither (Right a) = Result a

data Task = Task
    { runTask :: TaskInput -> TaskResult String
    , name :: String
    , autotoolName :: String
    , description :: String
    , longDescription :: String
    , parameters :: [(String,String)]
    , exampleInput :: String
    }

data TaskInput = TaskInput
    { input :: String
    , parallel :: Bool
    , limit :: Int
    }

readInput :: (Read a) => TaskInput -> Either String a
readInput = readEither . Task.input

readInputM :: (Read a) => TaskInput -> TaskResult a
readInputM input = case (readEither . Task.input) input of
    (Left _) -> fail "Could not read task description"
    (Right s) -> pure s

defaultTaskInput :: TaskInput
defaultTaskInput = TaskInput { input = undefined, parallel = True, limit = 250000 }

-- data TaskOutput = TaskOutput
--     { results :: Maybe [String]
--     , warnings :: Maybe [String]
--     , error :: Maybe String
--     }

-- defaultTaskOutput :: TaskOutput
-- defaultTaskOutput = TaskOutput { results = Nothing, warnings = Nothing, Task.error = Nothing }

-- fromResult :: String -> TaskOutput
-- fromResult r = defaultTaskOutput { results = Just [r] }

-- fromResults :: [String] -> TaskOutput
-- fromResults rs = defaultTaskOutput { results = Just rs }

-- fromEither :: Either String TaskOutput -> TaskOutput
-- fromEither (Left es) = defaultTaskOutput { Task.error = Just es }
-- fromEither (Right to) = to

shortDescribeTask :: Task -> String
shortDescribeTask t = unwords
    [ name t ++ replicate (10 - length (name t)) ' '
    -- , "(" ++ autotoolName t ++ ")" ++ replicate (20 - length (autotoolName t)) ' '
    , ":: "
    , description t
    ]

describeTask :: Task -> String
describeTask t = unlines $
    [ ""
    , "TASK NAME"
    , "\t" ++ name t
    , ""
    , "AUTOTOOL NAME"
    , "\t" ++ autotoolName t
    , ""
    , "DESCRIPTION"
    , unlines $ map ("\t"++) $ lines (longDescription t)
    , ""
    , "PARAMETERS"
    ]
    ++ params ++ 
    [ ""
    , "EXAMPLE INPUT"
    , ""
    , "```"
    , unpack $ pStringOpt showOpt (exampleInput t)
    , "```"
    ]
    where
        params = concatMap (\(k,v) -> [" - " ++ k,"\t" ++ v]) (parameters t)
        showOpt = defaultOutputOptionsNoColor {
            outputOptionsCompact = True,
            outputOptionsCompactParens = False,
            outputOptionsIndentAmount = 4
        }