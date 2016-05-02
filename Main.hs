-- Piotr Majcherczyk
-- pm334695

module Main where


import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad.Except
import Control.Monad.Trans.State

import qualified Data.Map as Map
import Data.Maybe

import CXBase

import LexCX
import ParCX
import SkelCX
import PrintCX
import AbsCX

import ErrM


-- Debug

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


-- Interpreter


type ParseFun a = [Token] -> Err a

type ES a = ExceptT String (StateT Env (IO)) a
data Status = Success | Error String deriving (Eq, Show)


execDecl :: Decl -> ES Status
execDecl _ = return Success
--execDecl (DeclDefault t is) = do
--    env <- get
--    mapM_ put (Map.insert is (defaultValue t) env) --TODO check if already there
--    return Success
--execDecl (DeclDefault t i e) = do
--    env <- get
--    value <- defaultValue t--eval e
--    put (Map.insert
--    return Success 


execFunctionDef :: FunctionDef -> ES Status
execFunctionDef _ = return Success


execExternalDecl :: ExternalDecl -> ES Status
execExternalDecl (GlobalFunction f) = (execFunctionDef f)
execExternalDecl (GlobalDecl d) = (execDecl d)


execTranslationUnit :: TranslationUnit -> ES Status
execTranslationUnit (Program externalDecl) = do
    mapM_ execExternalDecl externalDecl
    return Success


runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v


run :: Verbosity -> String -> IO ()
run v s = do
    let ts = myLexer s in case pTranslationUnit ts of
        Bad e -> do
            putStrLn "\nParse Failed...\n"
            putStrV v "Tokens:"
            putStrV v $ show ts
            putStrLn e
            exitFailure
        Ok p -> do
            putStrLn "\nParse Successful!"
            showTree v p
            res <- (runStateT (runExceptT $ execTranslationUnit p) Map.empty)
            case res of
                (Left e, _) -> do
                    print ("Runtime error: " ++ e)
                    exitFailure
                (Right r, _) -> do
                    print ("Env:", r)
                    exitSuccess


-- Main

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Parse stdin verbosely."
        , "  (files)         Parse content of files verbosely."
        , "  -s (files)      Silent mode. Parse content of files silently."
        ]
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        []         -> hGetContents stdin >>= run 2
        "-s":fs    -> mapM_ (runFile 0) fs
        fs         -> mapM_ (runFile 2) fs
