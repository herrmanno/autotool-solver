-- import System.Environment (getArgs)
-- import App (app, help)

-- main = do
--     args <- getArgs
--     go args >>= putStrLn
--     where
--         go [command,filename] = do
--             input <- readFile filename
--             return $ app command input
--         go _ = return help

import Prelude hiding ((+), (-), (*))
import Data.Function (on)
import qualified Data.Set as S
import Autotool.Data.Graph
import Autotool.Data.GraphOp ( (+), (*), co )
import Autotool.Data.LazyTree (trees, evalTree, showTree,  Tree(Node), Op(Op0), findTree)
import Autotool.Solver.Graphs (solve)

main = do
        let
            t = mkGraph [0..15] [ kante 0 2
                       , kante 0 3
                       , kante 0 4
                       , kante 0 5
                       , kante 0 6
                       , kante 1 3
                       , kante 1 4
                       , kante 1 5
                       , kante 1 6
                       , kante 2 3
                       , kante 2 4
                       , kante 2 5
                       , kante 2 6
                       , kante 3 4
                       , kante 3 6
                       , kante 4 5
                       , kante 5 6
                       , kante 7 8
                       , kante 8 9
                       , kante 9 10
                       , kante 11 12
                       , kante 11 13
                       , kante 12 13
                       ] 
            ks = map (\i -> Op0 ("K" ++ show i) (mkK i)) [1..5]
            ps = map (\i -> Op0 ("P" ++ show i) (mkP i)) [3..5]
            cs = map (\i -> Op0 ("C" ++ show i) (mkC i)) [3..5]
            ops = ks ++ ps ++ cs ++ [co, (+), (*)]
            ts = trees ops
        -- mapM_ (putStrLn . showTree) (take 1000000 ts)
        putStrLn $ showTree $ solve ops t
        -- print $ map normalize $ bipartSubgraphs t

