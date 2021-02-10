import System.Environment (getArgs)
import App (app, taskHelp, help)
import Control.Exception (catch, handle, SomeException(..))
import Task (TaskInput(input), defaultTaskInput, TaskResult(..))

main = do
    args <- getArgs
    out <- catch (go args) onError
    putStrLn out
    where
        go ["help", taskname] = return $ taskHelp taskname
        go [command,filename] = do
            input <- readFile filename
            let taskInput = defaultTaskInput { Task.input = input }
            return $ formatTaskOutput $ app command taskInput
        go _ = return help
        onError e = return $ "ERROR:\r\n\t" ++ show (e::Control.Exception.SomeException)

formatTaskOutput :: TaskResult String -> String
formatTaskOutput (Error e) = "ERROR:\r\n\t" ++ e
formatTaskOutput (Result r) = "RESULT:\r\n\t" ++ r
formatTaskOutput (Results rs) = "RESULTS:\r\n" ++ unlines (map ("\t"++) rs)
