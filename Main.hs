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

type ES a = ExceptT String (StateT Cont (IO)) a
data Status = Success | Error String deriving (Eq, Show)

type ParseFun a = [Token] -> Err a


type Cont = (Env, Store, FSpec, RetV)

emptyCont :: Cont
emptyCont = (Map.empty, Map.empty, Map.empty, TVoid)


-- Expressions

emptyExp :: Exp
emptyExp = ExpConstant $ ExpBool ConstantTrue


evalExp' :: Exp -> (DataType -> DataType) -> ES DataType
evalExp' e f = do
    v <- evalExp e
    return $ f v


evalExp'' :: Exp -> Exp -> (DataType -> DataType -> DataType) -> ES DataType
evalExp'' e1 e2 f = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    ev1 <- unref v1
    ev2 <- unref v2
    return $ f ev1 ev2


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


--TODO catch Error

evalExp :: Exp -> ES DataType
evalExp (ExpAssign e1 op e2) = do
    r <- evalExp e1
    case r of
      (TRef loc) -> do
          rv <- unref r
          v <- evalExp e2
          kaka <- findVal loc
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
evalExp (ExpUnaryInc (ExpConstant (ExpId id))) = applyToVar id einc
evalExp (ExpUnaryDec (ExpConstant (ExpId id))) = applyToVar id edec
evalExp (ExpPostInc uop (ExpConstant (ExpId id))) = do
    v <- findVar id
    return (evalUnaryOp v uop) where
        evalUnaryOp :: DataType -> UnaryOp -> DataType
        evalUnaryOp v OpUnaryPos = upos v
        evalUnaryOp v OpUnaryNeg = uneg v
        evalUnaryOp v OpUnaryNot = unot v
        evalUnaryOp v OpUnaryFlp = uflp v
evalExp (ExpFuncP f) = evalExp (ExpFuncPArgs f [])
evalExp (ExpFuncPArgs (ExpConstant (ExpId id)) args) = do
    a <- mapM evalExp args
    execFun id a
evalExp (ExpConstant c) = evalConstantType c where
    evalConstantType :: Constant -> ES DataType
    evalConstantType (ExpId id) = do
        loc <- findLoc id
        r <- flattenRef $ TRef loc
        return r
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
    (env, store, fspec, retv) <- lift get
    loc <- lift.lift $ newLoc store
    lift $ put (Map.insert (Ident id) loc env, Map.insert loc v store, fspec, retv)
    return Success


allocVar :: Ident -> DataType -> ES Status
allocVar (Ident id) v = do
    (env, _, _, _) <- lift get
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        doAllocVar (Ident id) v
    return Success


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
unref (TRef loc) = do
    v <- findVal loc
    return v
unref t = do
    return t


-- Flattens the chain of references
flattenRef :: DataType -> ES DataType
flattenRef (TRef loc) = do
    v <- findVal loc
    case v of
      (TRef _)  -> flattenRef v
      otherwise -> return $ TRef loc
flattenRef t = return t


setVal :: Loc -> DataType -> ES ()
setVal loc v = do
    (env, store, fspec, retv) <- lift get
    lift $ put (env, Map.insert loc v store, fspec, retv)


-- The variable must be previously allocated
setVar :: Ident -> DataType -> ES ()
setVar id v = do
    loc <- findLoc id
    setVal loc v


applyToVar :: Ident -> (DataType -> DataType) -> ES DataType
applyToVar id f = do
    v <- findVar id
    let v' = f v
    setVar id v'
    return v'


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
    (env, store, fspec, _) <- lift get
    lift $ put $ (env, store, fspec, v)
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

dataTypeToString :: DataType -> ES String
dataTypeToString TVoid = return "Invalid value"
dataTypeToString (TBool v) = return $ show v
dataTypeToString (TInt v) = return $ show v
dataTypeToString (TString s) = return s
dataTypeToString (TRef l) = do
    v <- findVal l
    dataTypeToString v


setArgRefs :: [Arg] -> [DataType] -> ES [DataType]
setArgRefs [] [] = return []
setArgRefs (a:args) (v:vs) =
    case a of
      (ArgVal _ _) -> do
        rv <- unref v
        t <- setArgRefs args vs
        return (rv:t)
      (ArgRef _ _) -> do
        rv <- flattenRef v
        t <- setArgRefs args vs
        return (rv:t)


allocArgs :: [Arg] -> [DataType] -> ES Status
allocArgs args vs = do
    refArgs <- setArgRefs args vs
    forceAllocVars (map extractId args) refArgs where
        extractId (ArgVal _ id) = id
        extractId (ArgRef _ id) = id


-- It's ugly, but looks like it's difficult to escape those monads
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
    (env, store, fspec, _) <- lift get
    case Map.lookup (Ident fname) env of
      Nothing   -> execBuiltinFun (Ident fname) args
      Just floc -> do
        case Map.lookup floc fspec of
          Nothing -> throwError $ "Internal error: function '" ++ fname ++ "'"
          Just (rType, argl, stmt) -> do
            if length argl /= length args
              then throwError $ "Invalid number of parameters. " ++ fname ++ " " ++ (show argl)
              else allocArgs argl args
            execCompoundStmt stmt -- TODO var cover/alloc
            (env', store', fspec', retv') <- lift get
            lift $ put (env, store', fspec, TVoid)
            case rType of
              (TypeVoid) -> return TVoid
              otherwise  ->
                case retv' of
                  TVoid     -> throwError $ "Failed to obtain return value from function: " ++ fname
                  otherwise -> return retv'


allocFun :: Ident -> TypeSpec -> [Arg] -> CompoundStmt -> ES Status
allocFun (Ident id) t args stmt = do
    (env, store, fspec, retv) <- lift get
    loc <- lift.lift $ newLoc store
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        lift $ put (Map.insert (Ident id) loc env,
                    Map.insert loc (TFun (Ident id)) store,
                    Map.insert loc (t, args, stmt) fspec,
                    retv)
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
    putStrLn "Running parser."
    let ts = myLexer s in case pTranslationUnit ts of
      Bad e -> do
        putStrLn "Parser error:"
        putStrLn e
        exitFailure
      Ok p -> do
        showTree v p

        putStrLn "Running type checking."
        res <- runStateT (runExceptT (checkTypes p)) (Map.empty, Map.empty, Map.empty, TypeVoid)
        case res of
          (Left e, _) -> do
            putStrLn $ "Type checking error:\n" ++ e
            exitFailure
          (Right r, _) -> return ()

        putStrLn "Collecting global names."
        res <- (runStateT (runExceptT $ execTranslationUnit p) emptyCont)
        case res of
          (Left e, _) -> do
            putStrLn $ "Runtime error: " ++ e
            exitFailure
          (Right r, (env, store, fspec, retv)) -> do
            putStrLn "Execute main program...\n"
            case Map.lookup (Ident "main") env of
              Nothing -> print ("'main' function not found.")
              Just _  -> do
                res <- runStateT (runExceptT $ execFun (Ident "main") []) (env, store, fspec, retv)
                case res of
                  (Left e, _) -> do
                    print ("Runtime error: " ++ e)
                    exitFailure
                  (Right r, (env, store, fspec, retv)) ->
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
