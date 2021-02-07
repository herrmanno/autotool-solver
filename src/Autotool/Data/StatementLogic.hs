{-# LANGUAGE AllowAmbiguousTypes #-}
module Autotool.Data.StatementLogic
    (
    -- * Statement and other types
      Statement(..)
    , VarID
    , Universe
    , Interpretation
    , StatementOp
    -- * operations
    , universe
    , interpretations
    , truthTable
    , showTruthTable
    , equiv
    , nequiv
    -- * Interpretable
    , Interpretable(..)
    , MinTerm
    , MaxTerm
    , minterms
    , maxterms
    , reduce
    -- * Oprators
    , true
    , false
    , var
    , (&&)
    , (||)
    , (!)
    , (-->)
    , (<-->)
    ) where

import Prelude hiding ((&&), (||))
import qualified Prelude as Pre ((&&), (||))
import qualified Data.Map as M
import Data.Tree ( Tree(Node), foldTree )
import Autotool.Data.LazyTree (evalTree', isOp0, Op, mkOp0, mkOp0C, mkOp1, mkOp2)
import Data.List (sort, find, nub)
import Data.Function (on)

-- * Statement and other types

type VarID = Char

type Universe = [VarID]

type Interpretation = M.Map VarID Bool

type TruthTable = M.Map Interpretation Bool

type StatementOp = Op Interpretation Bool

-- TODO: Tag Statement with a 'Satisfiable | Unsatisfiable | Tautology' flag
-- so that truthtable, minterms, maxterms only work on a satisfiable statement
newtype Statement = Statement { tree :: Tree StatementOp } deriving (Eq, Show)

-- * OPERATIONS

-- | Extracts a statement's universe from it's variables
--
-- >>> universe (Statement $ Node (Autotool.Data.StatementLogic.&&) [ Node (var 'a') [], Node (Autotool.Data.StatementLogic.&&) [ Node (var 'b') [], Node (var 'c') [] ] ])
-- "abc"
universe :: Statement -> Universe
universe = map head . foldTree f . tree
    where
        f leaf children
            | isVar leaf = [show leaf]
            | otherwise = nub $ concat children
        isVar l = isOp0 l Pre.&& (1 == length (show l)) -- TODO: find a better way to identiy leafs

-- | Return all possible interpretations for a given universe
--
-- >>> interpretations "abc"
-- [fromList [('a',False),('b',False),('c',False)],fromList [('a',True),('b',False),('c',False)],fromList [('a',False),('b',True),('c',False)],fromList [('a',True),('b',True),('c',False)],fromList [('a',False),('b',False),('c',True)],fromList [('a',True),('b',False),('c',True)],fromList [('a',False),('b',True),('c',True)],fromList [('a',True),('b',True),('c',True)]]
interpretations :: Universe -> [Interpretation]
interpretations [] = [M.singleton '_' False, M.singleton '_' True]
interpretations [x] = [M.singleton x False, M.singleton x True]
interpretations (x:xs) = do
    i <- interpretations xs
    [M.insert x False i, M.insert x True i]

-- | Compares two statement by their truth table (semantically equivalence)
--
-- >>> equiv (Statement $ Node (var 'a') []) ((Statement $ Node (Autotool.Data.StatementLogic.&&) [ Node (var 'a') [], Node (Autotool.Data.StatementLogic.||) [ Node (var 'a') [], Node (var 'b') [] ] ]))
-- True
-- 
-- >>> equiv (Statement $ Node (var 'a') []) ((Statement $ Node (Autotool.Data.StatementLogic.&&) [ Node (var 'a') [], Node (var 'b') [] ]))
-- False
--
-- >>> equiv (Statement $ Node true []) ((Statement $ Node (Autotool.Data.StatementLogic.||) [ Node (var 'a') [], Node (Autotool.Data.StatementLogic.!) [ Node (var 'a') [] ] ]))
-- True
equiv :: Statement -> Statement -> Bool
-- equiv = (==) `on` truthTable
equiv a b = case (tta, ttb) of
    (Left x, Left y)    -> x == y
    (Right x, Right y)  -> compareTables x y
    _                   -> False
    where
        tta = truthTable a
        ttb = truthTable b
        compareTables x y = all (\ix -> all (compareInterpretations x y ix) (M.keys y)) (M.keys x)
        compareInterpretations tta ttb ia ib = case () of
            _ | ia `subInterpretationOf` ib -> tta M.! ia == ttb M.! ib
              | ib `subInterpretationOf` ia -> tta M.! ia == ttb M.! ib
              | otherwise                   -> True

-- | Compares two statement by their truth table (semantically equivalence)
nequiv :: Statement -> Statement -> Bool
nequiv a b = not $ equiv a b

-- | Generates a statement's truthtable or a Bool, if the statement is free of variables
--
-- >>> truthTable (Statement $ Node (Autotool.Data.StatementLogic.||) [ Node (var 'a') [], Node (var 'b') [] ])
-- Right (fromList [(fromList [('a',False),('b',False)],False),(fromList [('a',False),('b',True)],True),(fromList [('a',True),('b',False)],True),(fromList [('a',True),('b',True)],True)])
--
-- >>> truthTable (Statement $ Node (false) [])
-- Left False
--
-- >>> truthTable (Statement $ Node (true) [])
-- Left True
--
truthTable :: Statement -> Either Bool TruthTable
truthTable s = case universe s of
        "" -> Left $ evalTree' M.empty (tree s)
        u -> let t = tree s
                 is = interpretations u
                 vals = map (`evalTree'` t) is
                 tt = M.fromList (zip is vals)
                 in case () of
                     _  | and vals -> Left True
                        | all not vals -> Left False
                        | otherwise -> Right tt

showTruthTable :: TruthTable -> String
showTruthTable mod = unlines $ unwords (map (:[]) vars) : values
    where
        vars = sort $ M.keys $ fst $ M.findMin mod
        values = map valueRow (M.toList mod)
        valueRow (i,b) = unwords $ map (show . fromEnum . (i M.!)) vars ++ [show $ fromEnum b]

subInterpretationOf :: Interpretation -> Interpretation -> Bool
a `subInterpretationOf` b = all (\k -> M.lookup k a == M.lookup k b) (M.keys a)

-- * INTERPRETABLE

-- | A typed kind of Interpretation.
--
-- Used to distinguish MinTerm and MaxTerm in places where only one or ther other can be used.
class Interpretable a where
      runInterpretation :: a -> Interpretation
      mkInterpretable :: Interpretation -> a
      onInterpretation :: (Interpretation -> Interpretation) -> a -> a
      onInterpretation f a = mkInterpretable $ f $ runInterpretation a
      onInterpretation2 :: (Interpretation -> Interpretation -> Interpretation) -> a -> a -> a
      onInterpretation2 f a b = mkInterpretable $ f (runInterpretation a) (runInterpretation b)
      onInterpretable :: (Interpretation -> b) -> a -> b
      onInterpretable f a = f $ runInterpretation a

newtype MinTerm = MinTerm { getMinTerm :: Interpretation } deriving (Show)

instance Interpretable MinTerm where
      runInterpretation = getMinTerm
      mkInterpretable = MinTerm

newtype MaxTerm = MaxTerm { getMaxTerm :: Interpretation } deriving (Show)

instance Interpretable MaxTerm where
      runInterpretation = getMaxTerm
      mkInterpretable = MaxTerm

-- | Return the min terms of a statement (the Interpretations, where the Statement evaluates to `True`)
--
-- >>> minterms (toValue (read "(a && ! b) || (!a && b)" :: DAO.Statement) :: Statement)
-- [MinTerm {getMinTerm = fromList [('a',False),('b',True)]},MinTerm {getMinTerm = fromList [('a',True),('b',False)]}]
minterms :: Statement -> Either Bool [MinTerm]
minterms = fmap (map MinTerm . M.keys . M.filterWithKey (const id)) . truthTable

-- | Return the max terms of a statement (the Interpretations, where the Statement evaluates to `False`)
--
-- >>> maxterms (toValue (read "(a && ! b) || (!a && b)" :: DAO.Statement) :: Statement)
-- [MaxTerm {getMaxTerm = fromList [('a',False),('b',False)]},MaxTerm {getMaxTerm = fromList [('a',True),('b',True)]}]
maxterms :: Statement -> Either Bool [MaxTerm]
maxterms = fmap (map MaxTerm . M.keys . M.filterWithKey (const not)) . truthTable

-- | Reduces a set of Interpretables by merging pairs of interpretations which differ only in one var
--
-- >>> reduce $ map MinTerm [M.fromList [('a',True),('b',True),('c',True)], M.fromList [('a',True),('b',True),('c',False)], M.fromList [('b',True),('c',True)] ]
-- [MinTerm {getMinTerm = fromList [('b',True),('c',True)]},MinTerm {getMinTerm = fromList [('a',True),('b',True)]}]
reduce :: Interpretable a => [a] -> [a]
reduce [] = []
reduce xs
      | all ((>1) . length . runInterpretation) (diffs xs) = xs
      | otherwise = reduce $ foldr f [] xs
      where f i is = let others = filter ((>1) . M.size . runInterpretation . diff i) is
                         similiars = filter ((==1) . M.size . runInterpretation . diff i) is
                         reduced = case similiars of
                               [] -> [i]
                               _  -> map (merge i) similiars
                     in others ++ reduced

-- | Merges to Interpretables by subtracting their difference from the first one
--
-- >>> merge (MinTerm $ M.fromList [('a', True), ('b', True)]) (MinTerm $ M.fromList [('a', True), ('b', False)])
-- MinTerm {getMinTerm = fromList [('a',True)]}
merge :: Interpretable a => a -> a -> a
merge a b = M.difference (runInterpretation a) `onInterpretation` diff a b

-- | Returns all diffs from all combinations of the given interpretations
diffs :: Interpretable a => [a] -> [a]
diffs [] = []
diffs (x:xs) = map (diff x) xs ++ diffs xs

-- | Returns a sub interpretation, containing only the keys where a and b have different values
--
-- >>> diff (MinTerm $ M.fromList [('a', True), ('b', True)]) (MinTerm $ M.fromList [('a', True), ('b', False)])
-- MinTerm {getMinTerm = fromList [('b',True)]}
diff :: Interpretable a => a -> a -> a
diff = onInterpretation2 go
    where
        go a b = M.differenceWith f a b `M.union` M.differenceWith f b a
        f a b | a == b = Nothing
              | otherwise = Just a

-- * OPERATORS

true :: Op Interpretation Bool
true = mkOp0 "true" True

false :: Op Interpretation Bool
false = mkOp0 "false" False

var :: VarID -> Op Interpretation Bool
var c = mkOp0C [c] (M.! c)

(!) :: Op Interpretation Bool
(!) = mkOp1 "!" not

(&&) :: Op Interpretation Bool
(&&) = mkOp2 "&&" True (Pre.&&)

(||) :: Op Interpretation Bool
(||) = mkOp2 "||" True (Pre.||)

(-->) :: Op Interpretation Bool
(-->) = mkOp2 "->" True implication
    where implication a b = not a Pre.|| b

(<-->) :: Op Interpretation Bool
(<-->) = mkOp2 "<->" True equivalence
    where equivalence a b = a == b
