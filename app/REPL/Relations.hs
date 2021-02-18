{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL.Relations where

import Prelude hiding ((+), (-), (*))
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Control.Monad (join)
import Data.Tree (Tree(Node))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import System.Console.Haskeline (runInputT, defaultSettings, InputT, getInputLine, outputStrLn)
import Control.Exception.Base (Exception, catch, evaluate, throw)
import Control.Monad.IO.Class (liftIO)
import Autotool.Readable (Readable(..), spacedString, spaced, openPar, closePar)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Set as DAO
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.Binding as DAO
import Autotool.Data.RelOp ((+), (-), (&), (*), inverse, transitiveClosure, reflexiveClosure, RelOpContext(..))
import Autotool.Data.LazyTree (mkOp0C, Op, showTree, evalTree')
import Repl (Repl(..))

repl :: Repl
repl = Repl
    { name = "rels"
    , description = "Evaluate expressions on relations"
    , loop = replFn
    }

type Relation a = S.Set (a,a)

data Context a = Context { _universe :: [a], variables :: M.Map Char (Relation a) }

newtype ReplException = UndefinedVar Char deriving (Show)

instance Exception ReplException

getVariable c k = case M.lookup k (variables c) of
    Just v -> v
    Nothing -> throw $ UndefinedVar k

addVariable :: Char -> Relation a -> Context a -> Context a
addVariable k v (Context uni vars) = Context uni (M.insert k v vars)

setUniverse :: [a] -> Context a -> Context a
setUniverse u (Context _ vars) = Context u vars

instance (RelOpContext a) (Context a) where
    universe (Context u _) = u

data Term a = B (DAO.Binding (Relation a)) | U [a] | T (Tree (Op (Context a) (Relation a)))

instance (Show a) => Show (Term a) where
    show (B b) = show b
    show (T t) = showTree t

instance {-# OVERLAPS #-} (Ord a, Readable a) => Read (Term a) where
    readsPrec _ = P.readP_to_S readP

instance (Ord a, Readable a) => Readable (Term a) where
    readP =
            T <$> expr0
        <|> B . fmap toValue <$> (readP :: ReadP (DAO.Binding (DAO.Set (a,a))))
        <|> U <$> (spacedString "universe" *> readP)
        where
            expr0 = P.chainl1 expr1 (spacedString "&" $> node2 (&))
            expr1 = P.chainl1 expr2 (spacedString "+" $> node2 (+))
            expr2 = P.chainl1 expr3 (spacedString "-" $> node2 (-))
            expr3 = P.chainl1 term (spacedString "." $> node2 (*))
            term =
                    variable
                <|> node1 inverse <$> (spacedString "inverse" *> term)
                <|> node1 transitiveClosure <$> (spacedString "transitive_cl" *> term)
                <|> node1 reflexiveClosure <$> (spacedString "reflexive_cl" *> term)
                <|> spaced (openPar *> expr0 <* closePar)
            variable = node0 . var <$> spaced (P.satisfy (`elem` ['A'..'Z']))
            var name = mkOp0C [name] (`getVariable` name)
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]

replFn :: IO ()
replFn = runInputT defaultSettings (help >> loop (Context [] M.empty))
    where
        help = outputStrLn $ unlines
            [ "HELP"
            , "  Define new sets relations:     S = {(1,2), (3,4)}"
            , "  Define universe:               universe [1,2,3,4]"
            , "  Evaluate Expressions like:     (S + R) . (S - R)"
            , ""
            , "  Available operators:"
            , "  - &"
            , "  - +"
            , "  - -"
            , "  - ."
            , "  - inverse (needs defined universe)"
            , "  - transitive_cl"
            , "  - reflexive_cl"
            ]
        loop :: Context Int -> InputT IO ()
        loop c = do
            line <- getInputLine "relations > "
            let term = readMaybe <$> line
            case join term of
                Nothing -> loop c
                (Just (B b)) -> let (k,v) = DAO.toPair b in loop (addVariable (DAO.fromId k) v c)
                (Just (U u)) -> loop (setUniverse u c)
                (Just (T t)) -> liftIO (eval c t)
                                >>= \case
                                    (Right val) -> return $ show (toValue val :: DAO.Set (Int,Int))
                                    (Left err) -> return err
                                >>= outputStrLn
                                >> loop c
        eval c t = catch (Right <$> evaluate (evalTree' c t)) onError
        onError (UndefinedVar k) = return $ Left $ "Error: Undefined variable " ++ show k