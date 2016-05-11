-- Piotr Majcherczyk
-- pm334695

module Main where


import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)
import System.Exit

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


evalAssignOp :: DataType -> DataType -> AssignmentOp -> DataType
evalAssignOp v1 v2 OpAssign = v2
evalAssignOp v1 v2 OpAssignMul = emul v1 v2
evalAssignOp v1 v2 OpAssignDiv = ediv v1 v2
evalAssignOp v1 v2 OpAssignMod = emod v1 v2
evalAssignOp v1 v2 OpAssignAdd = eadd v1 v2
evalAssignOp v1 v2 OpAssignSub = esub v1 v2
evalAssignOp v1 v2 OpAssignAnd = elogand v1 v2
evalAssignOp v1 v2 OpAssignXor = elogxor v1 v2
evalAssignOp v1 v2 OpAssignOr  = elogor v1 v2


evalExp :: Exp -> ES DataType
evalExp (ExpAssign (ExpConstant (ExpId (Ident id))) op e2) = do
    v1 <- evalExp $ ExpConstant $ ExpId $ Ident id
    v2 <- evalExp e2
    --TODO references
    (env, store, fargs, local) <- lift get
    case Map.lookup (Ident id) env of
        Nothing -> throwError $ "Unknown variable: " ++ id
        Just vloc -> do
            lift $ put (env, Map.insert vloc v store, fargs, local)
            return v
            where
                v = evalAssignOp v1 v2 op
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
evalExp (ExpPostInc uop e) = return TVoid
evalExp (ExpFuncP f) = evalExp (ExpFuncPArgs f [])
evalExp (ExpFuncPArgs (ExpConstant (ExpId id)) args) = do
    a <- mapM evalExp args
    execFun id a
evalExp (ExpConstant c) = evalConstantType c where
    evalConstantType :: Constant -> ES DataType
    evalConstantType (ExpId (Ident id)) = do
        (env, store, _, _) <- lift get
        case Map.lookup (Ident id) env of
            Nothing -> throwError $ "Unknown variable: " ++ id
            Just vlocation -> do
                case Map.lookup vlocation store of
                    Nothing    -> throwError $ "Internal error: variable " ++ id
                    Just value -> return value
    evalConstantType (ExpInt v) = return $ TInt v
    evalConstantType (ExpBool ConstantTrue) = return $ TBool True
    evalConstantType (ExpBool ConstantFalse) = return $ TBool False
    evalConstantType (ExpString s) = return $ TString s
evalExp exp = throwError $ "Internal error: expression " ++ (show exp)


-- Declarations

newLoc :: Store -> IO Loc
newLoc s =
    return $ (maximum $ k s) + 1 where
        k s | null (keys s) = [0]
            | otherwise     = keys s where
            keys s = Map.keys s


doAllocVar :: Ident -> DataType -> ES Status
doAllocVar (Ident id) v = do
    (env, store, fargs, local) <- lift get
    loc <- lift.lift $ newLoc store
    lift $ put (Map.insert (Ident id) loc env, Map.insert loc v store, fargs, local)
    return Success


allocVar :: Ident -> DataType -> ES Status
allocVar (Ident id) v = do
    (env, store, fargs, local) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        doAllocVar (Ident id) v
    liftIO $ putStrLn ("Variable " ++ id ++ " allocated")
    return Success


-- These functions is supposed to be called with lists of exact same length.
allocVars :: [Ident] -> [DataType] -> ES Status
allocVars [] [] = return Success
allocVars (id:ids) (v:vs) = do
    allocVar id v
    allocVars ids vs

forceAllocVars :: [Ident] -> [DataType] -> ES Status
forceAllocVars [] [] = return Success
forceAllocVars (id:ids) (v:vs) = do
    doAllocVar id v
    forceAllocVars ids vs


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

--TODO use this function
findFun :: Ident -> ES CompoundStmt
findFun id = do
    (env, _, fargs, _) <- lift get
    case Map.lookup id env of
        Nothing -> throwError $ "Unknown function name: " ++ (show id)
        Just floc -> do
            case Map.lookup floc fargs of
                Nothing ->
                    throwError $ "Internal function error: " ++ (show id)
                Just (_, stmt) -> return stmt


dataTypeToString :: DataType -> String
dataTypeToString TVoid = "Invalid value"
dataTypeToString (TBool v) = show v
dataTypeToString (TInt v) = show v
dataTypeToString (TString s) = s
dataTypeToString (TRef t l) = dataTypeToString $ unref (TRef t l)


allocArgs :: [Arg] -> [DataType] -> ES Status
allocArgs args vs =
    forceAllocVars (map extractId args) vs where
        extractId (ArgVal t id) = id
-- TODO ref


--valueToString :: Exp -> ES String
--valueToString e = do
--    v <- evalExp e
--    return dataTypeToString


execBuiltinFun :: Ident -> [DataType] -> ES Status
execBuiltinFun (Ident s) args =
    case s of
        "print"   -> do
            liftIO $ mapM_ (putStrLn . dataTypeToString) args
            return Success
        otherwise -> throwError $ "Unknown built-in function name: " ++ s


execFun :: Ident -> [DataType] -> ES DataType
execFun (Ident fname) args = do
    (env, store, fargs, _) <- lift get
    case Map.lookup (Ident fname) env of
        Nothing   -> do
            fstatus <- execBuiltinFun (Ident fname) args
            case fstatus of
                Error _ -> throwError $ "Unknown function name: " ++ fname
                Success -> return TVoid --TODO generalise
        Just floc -> do
            case Map.lookup floc fargs of
                Nothing -> throwError $ "Internal function error: " ++ fname
                Just (argl, stmt) -> do
                    if length argl /= length args
                        then throwError $ "Invalid number of parameters. " ++ fname ++ " " ++ (show argl)
                        else allocArgs argl args
                    execCompoundStmt stmt -- TODO var cover/alloc
                    (env', store', fargs', ret') <- lift get
                    lift $ put (env, store', fargs, TVoid)
                    case ret' of
                        TVoid     -> throwError $ "Failed to obtain return value from function: " ++ fname
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
                    Map.insert loc (args, stmt) fargs,
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
                            res <- runStateT (runExceptT $ execFun (Ident "main") []) (env, store, fargs, loc)
                            case res of
                                (Left e, _) -> do
                                    print ("Runtime error: " ++ e)
                                    exitFailure
                                (Right r, (env, store, fargs, loc)) ->
                                    case r of
                                        (TInt 0) -> do
                                            putStrLn "Program successfully finished."
                                            exitSuccess
                                        (TInt k) -> do
                                            putStrLn $ "Program finished with error code " ++ (show k) ++ "."
                                            exitWith $ ExitFailure (fromIntegral k)
                                        otherwise -> do
                                            putStrLn $ "Program finished with invalid return value."
                                            exitFailure


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
