{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL.Sets (repl) where

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
import Control.Exception.Base (Exception, catch, throw, evaluate)
import Repl (Repl(..))
import Autotool.Readable (Readable(..), spacedString, spaced, openPar, closePar)
import Autotool.DAO (toValue)
import qualified Autotool.DAO.Identifier as DAO
import qualified Autotool.DAO.NestedSet as DAO
import qualified Autotool.DAO.Binding as DAO
import Autotool.Data.SetOp ((+), (-), (&), pow)
import Autotool.Data.LazyTree (mkOp0C, Op, showTree, evalTree')
import Autotool.Data.NestedSet (NSet)
import Control.Monad.IO.Class (liftIO)

repl :: Repl
repl = Repl
    { name = "sets"
    , description = "Evaluate expressions on sets"
    , loop = replFn
    }

data Term a = B (DAO.Binding (NSet a)) | T (Tree (Op (M.Map Char (NSet a)) (NSet a)))

newtype ReplException = UndefinedVar Char deriving (Show)

instance Exception ReplException

getVar c k = case M.lookup k c of
    (Just v) -> v
    Nothing -> throw $ UndefinedVar k

instance (Show a) => Show (Term a) where
    show (B b) = show b
    show (T t) = showTree t

instance {-# OVERLAPS #-} (Ord a, Readable a) => Read (Term a) where
    readsPrec _ = P.readP_to_S readP

instance (Ord a, Readable a) => Readable (Term a) where
    readP = T <$> expr0 <|> B . fmap toValue <$> (readP :: ReadP (DAO.Binding (DAO.NestedSet a)))
        where
            expr0 = P.chainl1 expr1 (spacedString "&" $> node2 (&))
            expr1 = P.chainl1 expr2 (spacedString "+" $> node2 (+))
            expr2 = P.chainl1 term (spacedString "-" $> node2 (-))
            term = variable <|> node1 pow <$> (spacedString "pow" *> term) <|> spaced (openPar *> expr0 <* closePar)
            variable = node0 . var <$> spaced (P.satisfy (`elem` ['A'..'Z']))
            var name = mkOp0C [name] (`getVar` name)
            node0 op = Node op []
            node1 op a = Node op [a]
            node2 op a b = Node op [a,b]

replFn :: IO ()
replFn = runInputT defaultSettings (help >> loop M.empty)
    where
        help = outputStrLn $ unlines
            [ "HELP"
            , "  Define new sets like:          S = {1,{2}, {3, {}}}"
            , "  Evaluate Expressions like:     (S + R) & (S - R)"
            , ""
            , "  Available operators (by ascending precedence):"
            , "  - &"
            , "  - +"
            , "  - -"
            , "  - pow"
            ]
        loop :: M.Map Char (NSet Int) -> InputT IO ()
        loop c = do
            line <- getInputLine "sets > "
            let term = readMaybe <$> line
            case join term of
                Nothing -> loop c
                (Just (B b)) -> let (k,v) = DAO.toPair b in loop (M.insert (DAO.fromId k) v c)
                (Just (T t)) -> liftIO (eval c t)
                                >>= \case 
                                    (Right val) -> return $ show (toValue val :: DAO.NestedSet Int)
                                    (Left err) -> return err
                                >>= outputStrLn
                                >> loop c
        eval c t = catch (Right <$> evaluate (evalTree' c t)) onError
        onError (UndefinedVar k) = return $ Left $ "Error: Undefined variable " ++ show k