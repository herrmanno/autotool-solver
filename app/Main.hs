import Autotool.Data.Set (S(..), insert, union, diff, intersect, compose, pow)
import Autotool.Data.Op (Op2(..), Op1(..))
import Autotool.Data.Tree (Tree(..), size, buildTrees)
import Autotool.Solver.Relations (solve)

main :: IO ()
main = do
    let a = Set "R" [ V(1 , 4), V(2 , 4), V(3 , 2), V(4 , 1) ] :: S (Int,Int)
    let b = Set "S" [ V(1 , 4), V(2 , 2), V(2 , 3), V(4 , 4) ]
    let r = S[ V(1 , 1) , V(1 , 4) , V(2 , 1) , V(2 , 2) , V(2 , 4) , V(4 , 1) , V(4 , 4) ]
    let op2s = [Add, And, Subtr, Compose]
    let op1s = []
    print $ solve op2s op1s [a,b] r 3
