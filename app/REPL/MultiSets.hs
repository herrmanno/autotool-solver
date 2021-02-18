{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL.MultiSets where

import Prelude hiding ((+), (-))
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Control.Monad (join)
import Data.Tree (Tree(Node))
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import System.Console.Haskeline (runInputT, defaultSettings, InputT, getInputLine, outputStrLn)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Base (Exception, catch, evaluate, throw)
import Repl (Repl(..))
import Autotool.Readable (Readable(..), spacedString, spaced, openPar, closePar)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.MultiSet as DAO
import qualified Autotool.DAO.Binding as DAO
import Autotool.Data.MultiSetOp ((+), (-), (&))
import Autotool.Data.LazyTree (mkOp0C, Op, showTree, evalTree')

repl :: Repl
repl = Repl
    { name = "multisets"
    , description = "Evaluate expressions on multi sets"
    , loop = replFn
    }

type MM a = M.Map a Int

newtype Context a = Context (M.Map Char (MM a)) deriving (Show)

newtype ReplException = UndefinedVar Char deriving (Show)

instance Exception ReplException

addVar :: Char -> MM a -> Context a -> Context a
addVar k v (Context m) = Context $ M.insert k v m

getVar :: Char -> Context a -> MM a
getVar k (Context m) = case M.lookup k m of
    Just v -> v
    Nothing -> throw $ UndefinedVar k

data Term a = B (DAO.Binding (MM a)) | T (Tree (Op (Context a) (MM a)))

instance (Show a) => Show (Term a) where
    show (B b) = show b
    show (T t) = showTree t

instance {-# OVERLAPS #-} (Ord a, Readable a) => Read (Term a) where
    readsPrec _ = P.readP_to_S readP

instance (Ord a, Readable a) => Readable (Term a) where
    readP = T <$> expr0 <|> B . fmap toValue <$> (readP :: ReadP (DAO.Binding (DAO.MultiSet a)))
        where
            expr0 = P.chainl1 expr1 (spacedString "&" $> node2 (&))
            expr1 = P.chainl1 expr2 (spacedString "+" $> node2 (+))
            expr2 = P.chainl1 term (spacedString "-" $> node2 (-))
            term = variable <|> spaced (openPar *> expr0 <* closePar)
            variable = node0 . var <$> spaced (P.satisfy (`elem` ['A'..'Z']))
            var name = mkOp0C [name] (getVar name)
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]

replFn :: IO ()
replFn = runInputT defaultSettings (help >> loop (Context M.empty))
    where
        help = outputStrLn $ unlines
            [ "HELP"
            , "  Define new multi sets like:    S = {q: 1, r: 2}"
            , "  Evaluate Expressions like:     (S + R) & (S - R)"
            , ""
            , "  Available operators (by ascending precedence):"
            , "  - &"
            , "  - +"
            , "  - -"
            ]
        loop :: Context DAO.Identifier -> InputT IO ()
        loop c = do
            line <- getInputLine "multisets > "
            let term = readMaybe <$> line
            case join term of
                Nothing -> loop c
                (Just (B b)) -> let (k,v) = DAO.toPair b in loop (addVar (DAO.fromId k) v c)
                (Just (T t)) -> liftIO (eval c t)
                                >>= \case
                                    (Right val) -> return $ show (toValue val :: DAO.MultiSet DAO.Identifier)
                                    (Left err) -> return err
                                >>= outputStrLn
                                >> loop c
        eval c t = catch (Right <$> evaluate (evalTree' c t)) onError
        onError (UndefinedVar k) = return $ Left $ "Error: Undefined variable " ++ show k