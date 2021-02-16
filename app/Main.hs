{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Control.Exception (catch, SomeException(..))
import Paths_autotool_solver as Mod ( version )
import Data.Version (showVersion)
import Data.FileEmbed (embedStringFile)
import Task (TaskInput(input), defaultTaskInput, TaskResult(..))
import App (app, taskHelp, taskTypeDescriptions)

main = do
    args <- getArgs
    out <- catch (go args) onError
    putStrLn out
    where
        go ["license"] = return $ $(embedStringFile "LICENSE")
        go ["version"] = return $ showVersion Mod.version
        go ["tasks"] = return $ unlines taskTypeDescriptions
        go ["help", taskname] = return $ taskHelp taskname
        go [command,filename] = do
            input <- readFile filename
            let taskInput = defaultTaskInput { Task.input = input }
            return $ formatTaskOutput $ app command taskInput
        go _ = return usage
        onError e = return $ "ERROR:\r\n\t" ++ show (e::Control.Exception.SomeException)

usage :: String 
usage = unlines
    [ "USAGE"
    , "  run task:              <task> <task description file>"
    , "  show task types:       tasks"
    , "  show task description: help <task>"
    , "  show usage:            help"
    , "  show version:          version"
    , "  show license:          license"
    ]

formatTaskOutput :: TaskResult String -> String
formatTaskOutput (Error e) = "ERROR:\r\n\t" ++ e
formatTaskOutput (Result r) = "RESULT:\r\n\t" ++ r
formatTaskOutput (Results rs) = "RESULTS:\r\n" ++ unlines (map ("\t"++) rs)
