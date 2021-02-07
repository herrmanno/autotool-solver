import System.Environment (getArgs)
import App (app, taskHelp, help)
import Control.Exception (catch, handle, SomeException(..))

main = do
    args <- getArgs
    out <- catch (go args) onError
    putStrLn out
    where
        go ["help", taskname] = return $ taskHelp taskname
        go [command,filename] = do
            input <- readFile filename
            return $ app command input
        go _ = return help
        onError e = return $ "ERROR\t" ++ show (e::Control.Exception.SomeException)