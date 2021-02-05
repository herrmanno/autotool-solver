import System.Environment (getArgs)
import App (app, help)

main = getArgs >>= go >>= putStrLn
    where
        go [command,filename] = do
            input <- readFile filename
            return $ app command input
        go _ = return help