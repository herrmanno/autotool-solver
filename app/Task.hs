module Task (Task(..), describeTask, shortDescribeTask) where

import Data.Text.Lazy (unpack)
import Text.Pretty.Simple ( pShow, pShowOpt, defaultOutputOptionsNoColor, OutputOptions(..), pStringOpt )

data Task = Task
    { runTask :: String -> String
    , name :: String
    , autotoolName :: String
    , description :: String
    , longDescription :: String
    , parameters :: [(String,String)]
    , exampleInput :: String
    }

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