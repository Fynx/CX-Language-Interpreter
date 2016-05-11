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


-- Expressions

evalExp' :: Exp -> (DataType -> DataType) -> ES DataType
evalExp' e f = do
    v <- evalExp e
    return $ f v


evalExp'' :: Exp -> Exp -> (DataType -> DataType -> DataType) -> ES DataType
evalExp'' e1 e2 f = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    return $ f v1 v2


evalExp :: Exp -> ES DataType
evalExp (ExpAssign e1 op e2) = return TVoid --TODO
evalExp (ExpCondition e1 e2 e3) = do
    (TBool c) <- evalExp e1
    if c
      then evalExp e2
      else evalExp e3
evalExp (ExpLogOr e1 e2) = evalExp'' e1 e2 elogor
evalExp (ExpLogAnd e1 e2) = evalExp'' e1 e2 elogand
evalExp (ExpEq e1 e2) = evalExp'' e1 e2 eeq
evalExp (ExpNeq e1 e2) = evalExp'' e1 e2 eneq
evalExp (ExpLt e1 e2) = evalExp'' e1 e2 elt
evalExp (ExpGt e1 e2) = evalExp'' e1 e2 egt
evalExp (ExpLe e1 e2) = evalExp'' e1 e2 ele
evalExp (ExpGe e1 e2) = evalExp'' e1 e2 ege
evalExp (ExpAdd e1 e2) = evalExp'' e1 e2 eadd
evalExp (ExpSub e1 e2) = evalExp'' e1 e2 esub
evalExp (ExpMul e1 e2) = evalExp'' e1 e2 emul
evalExp (ExpDiv e1 e2) = evalExp'' e1 e2 ediv
evalExp (ExpMod e1 e2) = evalExp'' e1 e2 emod
evalExp (ExpUnaryInc e) = evalExp' e einc
evalExp (ExpUnaryDec e) = evalExp' e edec
--    v <- evalExp e
--    return $ einc v
evalExp (ExpPostInc uop e) = return TVoid
evalExp (ExpFuncP f) = return TVoid--do
--    execFun f []
--    return (TInt 0) --TODO return value
evalExp (ExpFuncPArgs f args) = return TVoid--do
--    execFun f args
--    return (TInt 0) --TODO same here
evalExp (ExpConstant c) = return $ constantType c where
    constantType :: Constant -> DataType
    constantType (ExpId (Ident id)) = TString id
    constantType (ExpInt v) = TInt v
    constantType (ExpBool ConstantTrue) = TBool True
    constantType (ExpBool ConstantFalse) = TBool False
    constantType (ExpString s) = TString s


-- Declarations

newLoc :: Store -> IO Loc
newLoc s =
    return $ (maximum $ k s) + 1 where
        k s | null (keys s) = [0]
            | otherwise     = keys s where
            keys s = Map.keys s


allocVar :: Ident -> DataType -> ES Status
allocVar (Ident id) v = do
    (env, store, fargs, local) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        lift $ put (Map.insert (Ident id) loc env, Map.insert loc v store, fargs, local)
    liftIO $ putStrLn ("Variable " ++ id ++ " allocated")
    return Success


execDecl :: Decl -> ES Status
execDecl (DeclDefault t is) = do
    mapM_ (flip allocVar (defaultValue t)) is
    return Success
execDecl (DeclDefine t i e) = do
    v <- evalExp e
    allocVar i v
    return Success


-- Statements


execCompoundStmt :: CompoundStmt -> ES Status
execCompoundStmt (StmtCompoundList l) = do
    mapM_ execStmt l
    return Success
execCompoundStmt StmtCompoundEmpty = return Success


execSelectionStmt :: SelectionStmt -> ES Status
execSelectionStmt (StmtIf e s) = do
    (TBool c) <- evalExp e
    if c
      then do
        execCompoundStmt s
        return Success
      else
        return Success
execSelectionStmt (StmtIfElse e s1 s2) = do
    (TBool c) <- evalExp e
    if c
      then execCompoundStmt s1
      else execCompoundStmt s2
    return Success


--TODO move it!
emptyExp :: Exp
emptyExp = ExpConstant $ ExpBool ConstantTrue


execIterationStmt :: IterationStmt -> ES Status
execIterationStmt (StmtWhile e s) = do
    (TBool v) <- evalExp e
    if v
      then do
        execCompoundStmt s
        execIterationStmt (StmtWhile e s)
      else
        return Success
execIterationStmt (StmtFor1 epre cond epost s) = do
    _ <- evalExp epre
    execIterationStmt $ StmtWhile cond (whileBody s epost) where
        whileBody StmtCompoundEmpty s = StmtCompoundList [StmtExp s]
        whileBody (StmtCompoundList ss) s = StmtCompoundList $ (StmtExp s):ss
execIterationStmt (StmtFor2 epre cond s) =
    execIterationStmt $ StmtFor1 epre cond emptyExp s
execIterationStmt (StmtFor3 epre epost s) =
    execIterationStmt $ StmtFor1 epre emptyExp epost s
execIterationStmt (StmtFor4 cond epost s) =
    execIterationStmt $ StmtFor1 emptyExp cond epost s
execIterationStmt (StmtFor5 epost s) =
    execIterationStmt $ StmtFor1 emptyExp emptyExp epost s
execIterationStmt (StmtFor6 cond s) =
    execIterationStmt $ StmtFor1 emptyExp cond emptyExp s
execIterationStmt (StmtFor7 epre s) =
    execIterationStmt $ StmtFor1 epre emptyExp emptyExp s
execIterationStmt (StmtFor8 s) =
    execIterationStmt $ StmtFor1 emptyExp emptyExp emptyExp s


execReturnStmt :: Exp -> ES Status
execReturnStmt e = do
    v <- evalExp e
    (env, store, fargs, _) <- lift get
    lift $ put $ (env, store, fargs, v)
    return Success


execDeclStmt :: Decl -> ES Status
execDeclStmt d = execDecl d


execStmt :: Stmt -> ES Status
execStmt (StmtExp e) = do
    evalExp e
    return Success
execStmt (StmtCompound s) = execCompoundStmt s
execStmt (StmtSelection s) = execSelectionStmt s
execStmt (StmtIteration s) = execIterationStmt s
execStmt (StmtReturn e) = execReturnStmt e
execStmt (StmtDecl d) = execDeclStmt d


-- Functions

argTypes :: [Arg] -> [ArgType TypeSpec]
argTypes a =
    (foldl extractT [] a) where
        extractT l (ArgVal t id) = (Var t):l
        extractT l (ArgRef t id) = (Ref t):l


execFun :: Ident -> [DataType] -> ES DataType
execFun fname args = do
    (env, store, fargs, _) <- lift get
    case Map.lookup fname env of
        Nothing   -> throwError $ "Unknown function name: " ++ (show fname)
        Just floc -> do
            case Map.lookup floc fargs of
                Nothing        -> throwError $ "Internal function error: " ++ (show fname)
                Just (_, stmt) -> do
                    execCompoundStmt stmt -- TODO var cover/alloc
                    (env', store', fargs', ret') <- lift get
                    lift $ put (env, store', fargs, TVoid)
                    case ret' of
                        TVoid     -> throwError $ "Failed to obtain return value from: " ++ (show fname)
                        otherwise -> return ret'


allocFun :: Ident -> TypeSpec -> [Arg] -> CompoundStmt -> ES Status
allocFun (Ident id) t args stmt = do
    (env, store, fargs, local) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        lift $ put (Map.insert (Ident id) loc env,
                    Map.insert loc (TFun t) store,
                    Map.insert loc (argTypes args, stmt) fargs,
                    local)
    liftIO $ putStrLn ("Function " ++ id ++ " of type " ++ (show t) ++ " allocated.")
    return Success


execFunctionDef :: FunctionDef -> ES Status
execFunctionDef (FunctionArgsP t id args cstmt) = do
    allocFun id t args cstmt
    return Success
execFunctionDef (FunctionArgs t id args stmt) = do
    allocFun id t args (StmtCompoundList [stmt])
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
                (Right r, (env, store, fargs, loc)) -> do
                    print ("Env:   ", env)
                    print ("Store: ", store)
                    print ("Execute main program...")
                    case Map.lookup (Ident "main") env of
                        Nothing -> print ("'main' function not found.")
                        Just _  -> do
                            runStateT (runExceptT $ execFun (Ident "main") []) (env, store, fargs, loc)
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
