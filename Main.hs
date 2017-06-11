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

import Text.Read (readMaybe)

import CXBase
import CXTypeChecking

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

type ES a = ExceptT String (StateT FullEnv (IO)) a

type ParseFun a = [Token] -> Err a


type FullEnv = (Env, Store, FEnv, RetV)

emptyCont :: FullEnv
emptyCont = (Map.empty, Map.empty, Map.empty, TVoid)


-- Expressions

emptyExp :: Exp
emptyExp = ExpConstant $ ExpBool ConstantTrue


evalExp' :: Exp -> (DataType -> DataType) -> ES DataType
evalExp' e f = do
    v <- evalExp e
    ev <- unref v
    return $ f ev


evalExp'' :: Exp -> Exp -> (DataType -> DataType -> DataType) -> ES DataType
evalExp'' e1 e2 f = evalExpSafe'' e1 e2 f (\_ _ -> return ())


-- evalExp with checking the arguments
evalExpSafe'' :: Exp -> Exp -> (DataType -> DataType -> DataType) ->
                 (DataType -> DataType -> ES ()) -> ES DataType
evalExpSafe'' e1 e2 f check = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    ev1 <- unref v1
    ev2 <- unref v2
    _ <- check ev1 ev2
    return $ f ev1 ev2


checkZeroDivision :: DataType -> DataType -> ES ()
checkZeroDivision _ (TInt 0) = throwError $ "Division by zero"
checkZeroDivision _ _ = return ()


-- Different assignment operators, require and return an lvalue
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


-- Auxiliary functions for (++/--) operators, postfix and prefix
evalUnary :: Exp -> (DataType -> DataType) -> ES DataType
evalUnary e f = do
    r <- evalExp e
    case r of
      (TRef loc) -> do
        v <- unref r
        let nv = f v
        setVal loc nv
        return nv
      otherwise ->
        throwError $ "Expression " ++ (show e) ++ " is not an lvalue"
evalPostfix :: Exp -> (DataType -> DataType) -> ES DataType
evalPostfix e f = do
    r <- evalExp e
    case r of
      (TRef loc) -> do
        v <- unref r
        setVal loc (f v)
        return v
      otherwise ->
        throwError $ "Expression " ++ (show e) ++ " is not an lvalue"


evalExp :: Exp -> ES DataType
evalExp (ExpAssign e1 op e2) = do
    r <- evalExp e1
    case r of
      (TRef loc) -> do
        rv <- unref r
        v <- evalExp e2
        setVal loc (evalAssignOp rv v op)
        return r
      otherwise ->
        throwError $ "Expression " ++ (show e1) ++ " is not an lvalue"
evalExp (ExpCondition e1 e2 e3) = do
    (TBool c) <- evalExp e1
    if c
      then evalExp e2
      else evalExp e3
evalExp (ExpLogOr e1 e2) = evalExp'' e1 e2 elogor
evalExp (ExpLogXor e1 e2) = evalExp'' e1 e2 elogxor
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
evalExp (ExpDiv e1 e2) = evalExpSafe'' e1 e2 ediv checkZeroDivision
evalExp (ExpMod e1 e2) = evalExpSafe'' e1 e2 emod checkZeroDivision
evalExp (ExpUnaryInc e) = evalUnary e einc
evalExp (ExpUnaryDec e) = evalUnary e edec
evalExp (ExpPostInc e) = evalPostfix e einc
evalExp (ExpPostDec e) = evalPostfix e edec
evalExp (ExpUnaryOp uop e) = do
    v <- evalExp e
    ev <- unref v
    return (evalUnaryOp ev uop) where
        evalUnaryOp :: DataType -> UnaryOp -> DataType
        evalUnaryOp ev OpUnaryPos = upos ev
        evalUnaryOp ev OpUnaryNeg = uneg ev
        evalUnaryOp ev OpUnaryNot = unot ev
        evalUnaryOp ev OpUnaryFlp = uflp ev
evalExp (ExpFuncP f) = evalExp (ExpFuncPArgs f [])
evalExp (ExpFuncPArgs (ExpConstant (ExpId id)) args) = do
    a <- mapM evalExp args
    execFun id a
evalExp (ExpConstant c) = evalConstantType c where
    evalConstantType :: Constant -> ES DataType
    evalConstantType (ExpId id) = do
        loc <- findLoc id
        return (TRef loc)
    evalConstantType (ExpInt v) = return $ TInt v
    evalConstantType (ExpBool ConstantTrue) = return $ TBool True
    evalConstantType (ExpBool ConstantFalse) = return $ TBool False
    evalConstantType (ExpString s) = return $ TString s
evalExp exp = throwError $ "Internal error: expression " ++ (show exp)


-- Declarations

-- New location that is not in store, it's a pretty safe function, assuming the program
-- is not going to run out of integers while allocating variables
newLoc :: Store -> IO Loc
newLoc s =
    return $ (maximum $ k s) + 1 where
        k s | null (keys s) = [0]
            | otherwise     = keys s where
            keys s = Map.keys s

-- A whole lot of auxiliary functions

doAllocVar :: Ident -> DataType -> ES ()
doAllocVar (Ident id) v = do
    (env, store, fenv, retv) <- lift get
    loc <- lift.lift $ newLoc store
    lift $ put (Map.insert (Ident id) loc env, Map.insert loc v store, fenv, retv)
    return ()


allocVar :: Ident -> DataType -> ES ()
allocVar (Ident id) v = do
    (env, _, _, _) <- lift get
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        doAllocVar (Ident id) v


forceAllocVar = doAllocVar


findLoc :: Ident -> ES Loc
findLoc (Ident id) = do
    (env, _, _, _) <- lift get
    case Map.lookup (Ident id) env of
        Nothing -> throwError $ "Unknown variable: " ++ id
        Just vlocation -> return vlocation


findVal :: Loc -> ES DataType
findVal vloc = do
    (_, store, _, _) <- lift get
    case Map.lookup vloc store of
        Nothing    -> throwError $ "Internal error: location " ++ (show vloc)
        Just value -> return value


findVar :: Ident -> ES DataType
findVar id = do
     loc <- findLoc id
     findVal loc


unref :: DataType -> ES DataType
unref (TRef loc) = findVal loc
unref t = return t


setVal :: Loc -> DataType -> ES ()
setVal loc v = do
    (env, store, fspec, retv) <- lift get
    lift $ put (env, Map.insert loc v store, fspec, retv)


-- The variable must be previously allocated
setVar :: Ident -> DataType -> ES ()
setVar id v = do
    loc <- findLoc id
    setVal loc v


-- Applies a function to a variable
applyToVar :: Ident -> (DataType -> DataType) -> ES DataType
applyToVar id f = do
    v <- findVar id
    let v' = f v
    setVar id v'
    return v'


-- These functions are supposed to be called with lists of exact same length.
allocVars :: [Ident] -> [DataType] -> ES ()
allocVars [] [] = return ()
allocVars (id:ids) (v:vs) = do
    allocVar id v
    allocVars ids vs


-- Use this functions while allocating args for function (shadowing)
forceAllocVars :: [Ident] -> [DataType] -> ES ()
forceAllocVars [] [] = return ()
forceAllocVars (id:ids) (v:vs) = do
    forceAllocVar id v
    forceAllocVars ids vs


-- It's possible to assign an initial value only when declaring ONE variable
-- Incorrect: Int a = 5, b;
-- Correct:   Int a = 5; Int b;
execDecl :: Decl -> ES ()
execDecl (DeclDefault t is) = do
    mapM_ (flip allocVar (defaultValue t)) is
    return ()
execDecl (DeclDefine t i e) = do
    v <- evalExp e
    allocVar i v


-- Statements

-- Literally everything surrounded by {} parenthesis
-- It's possible to allocate variables inside, they're cleaned afterwards
execCompoundStmt :: CompoundStmt -> ES ()
execCompoundStmt (StmtCompoundList l) = do
    (env, store, fenv, _) <- lift get
    mapM_ execStmt l
    (_, store', _, retv) <- lift get
    lift $ put (env, store', fenv, retv)
    (_, store'', _, _) <- lift get
    lift $ put (env, store'', fenv, retv)
execCompoundStmt StmtCompoundEmpty = return ()


execSelectionStmt :: SelectionStmt -> ES ()
execSelectionStmt (StmtIf e s) = execSelectionStmt (StmtIfElse e s StmtCompoundEmpty)
execSelectionStmt (StmtIfElse e s1 s2) = do
    v <- evalExp e
    c <- unref v
    case c of
      (TBool True) -> execCompoundStmt s1
      otherwise    -> execCompoundStmt s2


-- while cond {}
-- for pre, cond, post {}
-- Do NOT use ';' like in C, it won't work
execIterationStmt :: IterationStmt -> ES ()
execIterationStmt (StmtWhile e s) = do
    (TBool v) <- evalExp e
    if v
      then do
        execCompoundStmt s
        execIterationStmt (StmtWhile e s)
      else
        return ()
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


-- Does NOT escape the function, only sets the return value
execReturnStmt :: Exp -> ES ()
execReturnStmt e = do
    v <- evalExp e
    (env, store, fenv, _) <- lift get
    lift $ put $ (env, store, fenv, v)
    return ()


execDeclStmt :: Decl -> ES ()
execDeclStmt d = execDecl d


execStmt :: Stmt -> ES ()
execStmt (StmtExp e) = evalExp e >> return ()
execStmt (StmtCompound s) = execCompoundStmt s
execStmt (StmtSelection s) = execSelectionStmt s
execStmt (StmtIteration s) = execIterationStmt s
execStmt (StmtReturn e) = execReturnStmt e
execStmt (StmtDecl d) = execDeclStmt d


-- Functions

dataTypeToString :: DataType -> ES String
dataTypeToString TVoid = return "Invalid value"
dataTypeToString (TBool v) = return $ show v
dataTypeToString (TInt v) = return $ show v
dataTypeToString (TString s) = return s
dataTypeToString (TRef l) = do
    v <- findVal l
    dataTypeToString v
dataTypeToString TFun = return "function" --"Function: " ++ show a ++ " -> " ++ show r


setArgRef :: (Arg, DataType) -> ES DataType
setArgRef ((ArgVal _ _), v) = unref v
setArgRef ((ArgRef _ _), v) = return v
setArgRef ((ArgFun _ _ _), v) = return v


allocArg :: (Ident, DataType) -> ES ()
allocArg (id, (TRef floc)) = do
    (env, store, fenv, retv) <- lift get
    lift $ put (Map.insert id floc env, store, fenv, retv)    
allocArg (id, v) = forceAllocVar id v


-- Nested functions can be arguments: Int f(String <Bool, Int <String, String> &> g)
-- the '&' after a function inside arg function is required
allocArgs :: [Arg] -> [DataType] -> ES ()
allocArgs args vs = do
    refs <- mapM setArgRef (combineLists args vs)
    mapM_ allocArg (combineLists ids refs)
    return ()
      where
        ids = map extractId args
        extractId (ArgVal _ id) = id
        extractId (ArgRef _ id) = id
        extractId (ArgFun _ _ id) = id


putStrValue :: DataType -> ES ()
putStrValue v = do
    s <- dataTypeToString v
    liftIO $ putStr s


execBuiltinFun :: Ident -> [DataType] -> ES DataType
execBuiltinFun (Ident s) args =
    case s of
      "print" -> do
        mapM_ putStrValue args
        liftIO $ putStrLn ""
        return TVoid
      "stringToInt" -> do
        arg <- singleValue args
        case arg of
          TString str -> case readMaybe str of
              Nothing -> throwError $ "Runtime error: invalid stringToInt conversion from '" ++ str ++ "'"
              Just v  -> return $ TInt v
          otherwise -> throwError $ "Internal error: typechecking stringToInt " ++ show otherwise
      "intToString" -> do
        arg <- singleValue args
        case arg of
          TInt v -> return $ TString $ show v
          otherwise -> throwError $ "Internal error: typechecking intToString"
      "stringToBool" -> do
        arg <- singleValue args
        case arg of
          TString "true"  -> return $ TBool True
          TString "false" -> return $ TBool False
          otherwise -> throwError $ "Invalid boolToString conversion from '" ++ show otherwise ++ "'"
      "boolToString" -> do
        arg <- singleValue args
        case arg of
          TBool True -> return $ TString $ "true"
          TBool False -> return $ TString $ "false"
          otherwise -> throwError $ "Internal error: typechecking boolToString"
      otherwise -> throwError $ "Unknown built-in function name: " ++ s
      where
        singleValue :: [DataType] -> ES DataType
        singleValue [x] = unref x
        singleValue _ = return TVoid


execFun :: Ident -> [DataType] -> ES DataType
execFun (Ident fname) args = do
    (env, store, fenv, retv) <- lift get
    case Map.lookup (Ident fname) env of
      Nothing   -> execBuiltinFun (Ident fname) args
      Just floc -> do
        case Map.lookup floc fenv of
          Nothing -> throwError $ "Internal error: function '" ++ fname ++ "'"
          Just (rType, argl, stmt, envf) -> do
            if length argl /= length args
              then throwError $ "Invalid number of parameters. " ++ fname ++ " " ++ (show argl)
              else lift $ put (envf, store, fenv, retv)
            allocArgs argl args
            execCompoundStmt stmt
            (_, store', _, retv') <- lift get
            lift $ put (env, store', fenv, TVoid)
            case rType of
              (TypeVoid) -> return TVoid
              otherwise  ->
                case retv' of
                  TVoid     -> throwError $ "Failed to obtain return value from function: " ++ fname
                  otherwise -> return retv'


argToTypeSpec :: Arg -> TypeSpec
argToTypeSpec (ArgVal t _) = t
argToTypeSpec (ArgRef t _) = t
argToTypeSpec (ArgFun t ts _) = (TypeFun t ts)


functionDefToType :: TypeSpec -> [Arg] -> TypeSpec
functionDefToType t args = (TypeFun t (map argToTypeSpec args))


allocFun :: Ident -> TypeSpec -> [Arg] -> CompoundStmt -> ES ()
allocFun (Ident id) t args stmt = do
    (env, store, fenv, retv) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else let
        env' = Map.insert (Ident id) loc env
        store' = Map.insert loc TFun store
        fenv' = Map.insert loc (t, args, stmt, env') fenv in
          lift $ put $ (env', store', fenv', retv)
    return ()


execFunctionDef :: FunctionDef -> ES ()
execFunctionDef (FunctionArgsP t id args cstmt) = do
    allocFun id t args cstmt
execFunctionDef (FunctionArgs t id args stmt) = do
    allocFun id t args (StmtCompoundList [stmt])
execFunctionDef (FunctionProcP t id cstmt) = execFunctionDef (FunctionArgsP t id [] cstmt)
execFunctionDef (FunctionProc t id stmt) = execFunctionDef (FunctionArgs t id [] stmt)


-- Main program

execExternalDecl :: ExternalDecl -> ES ()
execExternalDecl (GlobalFunction f) = (execFunctionDef f)
execExternalDecl (GlobalDecl d) = (execDecl d)


execTranslationUnit :: TranslationUnit -> ES ()
execTranslationUnit (Program externalDecl) = do
    mapM_ execExternalDecl externalDecl


runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v


-- Allows for a little bit of verbosity if required
putStrLnV :: Verbosity -> String -> IO ()
putStrLnV v s | v > 0     = putStrLn s
              | otherwise = return ()


run :: Verbosity -> String -> IO ()
run v s = do
    putStrLnV v "Running parser."
    let ts = myLexer s in case pTranslationUnit ts of
      Bad e -> do
        putStrLn "Parser error:"
        putStrLn e
        exitFailure
      Ok p -> do
        showTree v p

        putStrLnV v "Running type checking."
        res <- runStateT (runExceptT (checkTypes p)) (Map.empty, Map.empty, Map.empty, TypeVoid)
        case res of
          (Left e, _) -> do
            putStrLn $ "Type error:\n" ++ e
            exitFailure
          (Right r, _) -> return ()

        putStrLnV v "Collecting global names."
        res <- (runStateT (runExceptT $ execTranslationUnit p) emptyCont)
        case res of
          (Left e, _) -> do
            putStrLn $ "Runtime error:\n" ++ e
            exitFailure
          (Right r, (env, store, fenv, retv)) -> do
            putStrLnV v "Execute main program...\n"
            case Map.lookup (Ident "main") env of
              Nothing -> print "'main' function not found."
              Just _  -> do
                res <- runStateT (runExceptT $ execFun (Ident "main") []) (env, store, fenv, retv)
                case res of
                  (Left e, _) -> do
                    putStrLn $ "Runtime error: " ++ e
                    exitFailure
                  (Right r, (env, store, fenv, retv)) ->
                    case r of
                      (TInt 0) -> do
                        putStrLnV v "Program successfully finished.\n"
                        exitSuccess
                      (TInt k) -> do
                        putStrLn $ "Program finished with error code " ++ (show k) ++ ".\n"
                        exitWith $ ExitFailure (fromIntegral k)
                      otherwise -> do
                        putStrLn $ "Program finished with invalid return value.\n"
                        exitFailure


-- Main

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Parse stdin verbosely."
        , "  -v (files)      Parse content of files verbosely."
        , "  -s (files)      Silent mode. Parse content of files silently (default)."
        ]
    exitFailure


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        []         -> hGetContents stdin >>= run 2
        "-s":fs    -> mapM_ (runFile 0) fs
        "-v":fs    -> mapM_ (runFile 2) fs
        fs         -> mapM_ (runFile 0) fs
