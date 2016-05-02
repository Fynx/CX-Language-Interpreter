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

type ES a = ExceptT String (StateT Cont (IO)) a
data Status = Success | Error String deriving (Eq, Show)


--evalExp :: Exp -> DataType
--evalExp _ = return (TInt 5)


newLoc :: Store -> IO Loc
newLoc s =
    return $ (maximum $ k s) + 1 where
        k s | null (keys s) = [0]
            | otherwise     = keys s where
            keys s = Map.keys s


allocVar :: Ident -> DataType -> ES Status
allocVar (Ident id) v = do
    (env, store, fargs) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        lift $ put (Map.insert (Ident id) loc env, Map.insert loc v store, fargs)
    liftIO $ putStrLn ("Variable " ++ id ++ " allocated")
    return Success


execDecl :: Decl -> ES Status
execDecl (DeclDefault t is) = do
    mapM_ (flip allocVar (defaultValue t)) is
    return Success
execDecl (DeclDefine t i e) = do
    allocVar i (defaultValue t)--(evalExp e)
    return Success

-- Functions


argTypes :: [Arg] -> [ArgType TypeSpec]
argTypes a =
    (foldl extractT [] a) where
        extractT l (ArgVal t id) = (Var t):l
        extractT l (ArgRef t id) = (Ref t):l


allocFun :: Ident -> TypeSpec -> [Arg] -> ES Status
allocFun (Ident id) t args = do
    (env, store, fargs) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        lift $ put (Map.insert (Ident id) loc env,
                    Map.insert loc (TFun t) store,
                    Map.insert loc (argTypes args) fargs)
    liftIO $ putStrLn ("Function " ++ id ++ " of type " ++ (show t) ++ " allocated.")
    return Success


execFunctionDef :: FunctionDef -> ES Status
execFunctionDef (FunctionArgsP t id args cstmt) = do
    allocFun id t args
    --execCompoundStmt cstmt
    return Success
execFunctionDef (FunctionArgs t id args stmt) = do
    allocFun id t args
    --execStmt cstmt
    return Success
execFunctionDef (FunctionProcP t id cstmt) = execFunctionDef (FunctionArgsP t id [] cstmt)
execFunctionDef (FunctionProc t id stmt) = execFunctionDef (FunctionArgs t id [] stmt)


-- Main program

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
            res <- (runStateT (runExceptT $ execTranslationUnit p) emptyCont)
            case res of
                (Left e, _) -> do
                    print ("Runtime error: " ++ e)
                    exitFailure
                (Right r, (env, store, fargs)) -> do
                    print ("Env:   ", env)
                    print ("Store: ", store)
                    print ("FArgs: ", fargs)
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
