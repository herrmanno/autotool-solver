import System.Environment (getArgs)
import App (app, help)

main = do
    args <- getArgs
    go args >>= putStrLn
    where
        go [command,filename] = do
            input <- readFile filename
            return $ app command input
        go _ = return help

-- import Prelude hiding ((+), (-), (*))
-- import Autotool.Data.SetOp ( (&), (+), (-), pow )
-- import Autotool.Data.LazyTree (treesLevelCount, termsLength, trees, evalTree, showTreeFn, showTree,  Tree(Node), Op(Op0), findTree)
-- import Autotool.Solver.Sets (solve)
-- import Autotool.Parser.NestedSet (parseIntSet)
-- import Autotool.Data.Parallel.LazyTree (treesP)
-- import Autotool.Data.NestedSet (toStr)

-- main = do
--         let
--             a = Op0 "A" $ parseIntSet "{{}, {{}}}"
--             b = Op0 "B" $ parseIntSet "{1, {1}, {2, {}}}"
--             t = parseIntSet "{{}, {{}, {{}}}, {{{}}}}"
--             ops = [(+), (-), (&), pow, a, b]
--             result =
--                 Node (-) [
--                     Node pow [ Node a [] ],
--                     Node (-) [
--                         Node a [],
--                         Node pow [
--                             Node (&) [
--                                 Node a [],
--                                 Node b []
--                             ]
--                         ]
--                     ]
--                 ]
--             ts = trees ops
--         -- mapM_ (putStrLn . showTree) (take 50000 ts)
--         -- print $ let xs = [0..5] in zip xs $  map (treesLevelCount [2,1,3]) xs
--         -- print $ let xs = [600..700] in zip xs $  map (termsLength [2,1,3]) xs
--         -- print $ toStr $ evalTree result
--         putStrLn $ showTree $ solve ops t

