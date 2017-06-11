-- Piotr Majcherczyk
-- pm334695

module CXTypeChecking where


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


-- Containers & Monads

type TStore = Map.Map Loc TypeSpec
type RetType = TypeSpec

type TypeEnv = (Env, TStore, FEnv, RetType)

type TES a = ExceptT String (StateT TypeEnv (IO)) a


-- Declarations

ctTranslationUnit :: TranslationUnit -> TES ()
ctTranslationUnit (Program ds) = do
    mapM_ ctExternalDecl ds
    return ()


ctExternalDecl :: ExternalDecl -> TES ()
ctExternalDecl (GlobalFunction f) = ctFunctionDef f
ctExternalDecl (GlobalDecl d) = ctDecl d


ctFunctionDef :: FunctionDef -> TES ()
ctFunctionDef (FunctionArgsP ts id args cstmt) =
    allocFunT False id ts args cstmt >> ctFunction id
ctFunctionDef (FunctionArgs ts id args stmt) =
    allocFunT False id ts args (StmtCompoundList [stmt]) >> ctFunction id
ctFunctionDef (FunctionProcP t id cstmt) = ctFunctionDef (FunctionArgsP t id [] cstmt)
ctFunctionDef (FunctionProc t id stmt) = ctFunctionDef (FunctionArgs t id [] stmt)


allocFunT :: Bool -> Ident -> TypeSpec -> [Arg] -> CompoundStmt -> TES ()
allocFunT force (Ident id) t args stmt = do
    (env, tstore, fenv, rtype) <- lift get
    loc <- lift.lift $ newTLoc tstore
    if (Map.member (Ident id) env) && (not force)
      then
        throwError $ "Cannot create function '" ++ id ++ "', name already exists."
      else let
        env' = Map.insert (Ident id) loc env
        tstore' = Map.insert loc (fdefinitionToType (t, args, stmt, env')) tstore
        fenv' = Map.insert loc (t, args, stmt, env') fenv in
          lift $ put (env', tstore', fenv', rtype)


allocFunction :: Arg -> TES ()
allocFunction (ArgFun rv args id) = do
    allocFunT True id rv (map makeArg args) StmtCompoundEmpty where
        makeArg :: TypeSpec -> Arg
        makeArg (TypeFun r a) = (ArgFun r a (Ident ""))
        makeArg t = ArgVal t (Ident "")
allocFunction _ = return ()


ctFunction :: Ident -> TES ()
ctFunction id = do
    (env, tstore, fenv, _) <- lift get
    case Map.lookup id env of
      Nothing -> return ()
      Just loc -> do
        case Map.lookup loc fenv of
          Nothing -> return ()
          Just (ts, args, stmt, envf) -> do
            lift $ put (envf, tstore, fenv, TypeVoid)
            _ <- forceAllocVarsT (argsIds args) (argsTS args)
            mapM_ allocFunction args
            _ <- ctCompoundStmt stmt
            (_, _, _, rtype) <- lift get
            lift $ put (env, tstore, fenv, TypeVoid)
            if rtype == ts
              then return ()
              else throwError $ "Invalid return value of function " ++ showId id ++
                                "\n  Expected type: " ++ showTS ts ++ "\n  Actual type:   " ++
                                showTS rtype
            where
                argsIds [] = []
                argsIds ((ArgVal ts id):args) = id : argsIds args
                argsIds ((ArgRef ts id):args) = id : argsIds args
                argsIds ((ArgFun ts ats id):args) = id : argsIds args
                argsTS [] = []
                argsTS ((ArgVal ts id):args) = ts : argsTS args
                argsTS ((ArgRef ts id):args) = ts : argsTS args
                argsTS ((ArgFun ts ats id):args) = ts : argsTS args


ctDecl :: Decl -> TES ()
ctDecl (DeclDefault ts ids) = do
    mapM_ ((flip allocVarT) ts) ids
    return ()
ctDecl (DeclDefine ts id e) = do
    et <- ctExp e
    if ts == et
      then allocVarT id ts
      else throwError $ "Attempting to assign invalid value to variable '" ++ showId id ++ "'"


newTLoc :: TStore -> IO Loc
newTLoc s =
    return $ (maximum $ k s) + 1 where
        k s | null (keys s) = [0]
            | otherwise     = keys s where
            keys s = Map.keys s


doAllocVarT :: Ident -> TypeSpec -> TES ()
doAllocVarT (Ident id) t = do
    (env, tstore, fenv, rtype) <- lift get
    loc <- lift.lift $ newTLoc tstore
    lift $ put (Map.insert (Ident id) loc env, Map.insert loc t tstore, fenv, rtype)
    return ()


allocVarT :: Ident -> TypeSpec -> TES ()
allocVarT (Ident id) v = do
    (env, _, _, _) <- lift get
    if Map.member (Ident id) env
      then
        throwError $ "Name " ++ id ++ " already exists."
      else
        doAllocVarT (Ident id) v
    return ()


findLocT :: Ident -> TES Loc
findLocT (Ident id) = do
    (env, _, _, _) <- lift get
    case Map.lookup (Ident id) env of
        Nothing   -> throwError $ "Unknown variable: '" ++ id ++ "'"
        Just vloc -> return vloc


fdefinitionToType :: (TypeSpec, [Arg], CompoundStmt, Env) -> TypeSpec
fdefinitionToType (rv, args, _, _) =
    TypeFun rv (map argExtractType args)


argExtractType :: Arg -> TypeSpec
argExtractType (ArgVal t _) = t
argExtractType (ArgRef t _) = t
argExtractType (ArgFun r a _) = (TypeFun r a)


findValT :: Loc -> TES TypeSpec
findValT loc = do
    (_, tstore, fenv, _) <- lift get
    case Map.lookup loc tstore of
      Just v  -> return v
      Nothing -> throwError $ "Internal error: location " ++ (show loc)


findVarT :: Ident -> TES TypeSpec
findVarT id = do
    loc <- findLocT id
    findValT loc


allocVarsT :: [Ident] -> [TypeSpec] -> TES ()
allocVarsT [] [] = return ()
allocVarsT (id:ids) (v:vs) = do
    allocVarT id v
    allocVarsT ids vs


forceAllocVarsT :: [Ident] -> [TypeSpec] -> TES ()
forceAllocVarsT [] [] = return ()
forceAllocVarsT (id:ids) (v:vs) = do
    doAllocVarT id v
    forceAllocVarsT ids vs


-- Expressions


data Operation =
    As | AsMul | AsDiv | AsMod | AsAdd | AsSub | AsAnd | AsOr |
    LogAnd | LogOr | LogXor | Ieq | Neq | Lt | Gt | Le | Ge | Add | Sub | Mul | Div | Mod |
    UInc | UDec | PInc | PDec | UPos | UNeg | UNot | UFlp
        deriving Show


ctExp :: Exp -> TES TypeSpec
ctExp (ExpAssign e1 op e2) = ctExpAssign op e1 e2 where
    ctExpAssign :: AssignmentOp -> Exp -> Exp -> TES TypeSpec
    ctExpAssign OpAssign e1 e2 = canAExp2 As e1 e2
    ctExpAssign OpAssignMul e1 e2 = canAExp2 AsMul e1 e2
    ctExpAssign OpAssignDiv e1 e2 = canAExp2 AsDiv e1 e2
    ctExpAssign OpAssignMod e1 e2 = canAExp2 AsMod e1 e2
    ctExpAssign OpAssignAdd e1 e2 = canAExp2 AsAdd e1 e2
    ctExpAssign OpAssignSub e1 e2 = canAExp2 AsSub e1 e2
    ctExpAssign OpAssignAnd e1 e2 = canAExp2 AsAnd e1 e2
    ctExpAssign OpAssignOr e1 e2 = canAExp2 AsOr e1 e2
ctExp (ExpCondition cond e1 e2) = do
    ct <- ctExp cond
    if ct == TypeBool
      then return TypeBool
      else throwError $ "Non-boolean value in condition: " ++ show cond
ctExp (ExpLogOr e1 e2) = canAExp2 LogOr e1 e2
ctExp (ExpLogXor e1 e2) = canAExp2 LogXor e1 e2
ctExp (ExpLogAnd e1 e2) = canAExp2 LogAnd e1 e2
ctExp (ExpEq e1 e2) = canAExp2 Ieq e1 e2
ctExp (ExpNeq e1 e2) = canAExp2 Neq e1 e2
ctExp (ExpLt e1 e2) = canAExp2 Lt e1 e2
ctExp (ExpGt e1 e2) = canAExp2 Gt e1 e2
ctExp (ExpLe e1 e2) = canAExp2 Le e1 e2
ctExp (ExpGe e1 e2) = canAExp2 Ge e1 e2
ctExp (ExpAdd e1 e2) = canAExp2 Add e1 e2
ctExp (ExpSub e1 e2) = canAExp2 Sub e1 e2
ctExp (ExpMul e1 e2) = canAExp2 Mul e1 e2
ctExp (ExpDiv e1 e2) = canAExp2 Div e1 e2
ctExp (ExpMod e1 e2) = canAExp2 Mod e1 e2
ctExp (ExpUnaryInc e) = canAExp UInc e
ctExp (ExpUnaryDec e) = canAExp UDec e
ctExp (ExpPostInc e) = canAExp UInc e
ctExp (ExpPostDec e) = canAExp UDec e
ctExp (ExpUnaryOp uop e) = ctExpUnaryOp uop e where
    ctExpUnaryOp OpUnaryPos e = canAExp UPos e
    ctExpUnaryOp OpUnaryNeg e = canAExp UNeg e
    ctExpUnaryOp OpUnaryNot e = canAExp UNot e
    ctExpUnaryOp OpUnaryFlp e = canAExp UFlp e
ctExp (ExpFuncP e) = ctExp (ExpFuncPArgs e [])
ctExp (ExpFuncPArgs (ExpConstant (ExpId id)) args) = do
    (env, _, fenv, _) <- lift get
    argts <- mapM ctExp args
    case Map.lookup id env of
      Nothing -> do
        case id of -- built-in functions
          Ident "print" -> return TypeVoid
          Ident "intToString" ->
            case argts of
              [TypeInt] -> return TypeString
              otherwise -> throwError $ "Invalid argument of function 'intToString'"
          Ident "stringToInt" ->
            case argts of
              [TypeString] -> return TypeInt
              otherwise -> throwError $ "Invalid argument of function 'stringToInt'"
          Ident "boolToString" ->
            case argts of
              [TypeBool] -> return TypeString
              otherwise -> throwError $ "Invalid argument of function 'boolToString'"
          Ident "stringToBool" ->
            case argts of
              [TypeString] -> return TypeBool
              otherwise -> throwError $ "Invalid argument of function 'stringToBool'"
          otherwise -> throwError $ "Cannot find a function '" ++ showId id ++ "'"
      Just loc -> do
        case Map.lookup loc fenv of
          Nothing -> throwError $ "Internal error: cannot find a function '" ++ showId id ++ "'"
          Just (ts, rargs, _, _) -> do
            let rargts = map argExtractType rargs
            if compareArrays argts rargts
              then return ts
              else throwError $ "Arguments of function '" ++ showId id ++ "' do not match.\n" ++
                                "  Expected types: " ++ show rargts ++ "\n  Actual types: " ++ show argts
            where
                compareArrays [] [] = True
                compareArrays l [] = False
                compareArrays [] l = False
                compareArrays (x:xs) (y:ys)
                    | x == y    = compareArrays xs ys
                    | otherwise = False
ctExp (ExpFuncPArgs e _) = throwError $ "Expressions with functions hidden inside not implemented: " ++ show e
ctExp (ExpConstant c) = ctExpConstant c where
    ctExpConstant :: Constant -> TES TypeSpec
    ctExpConstant (ExpId id) = findVarT id
    ctExpConstant (ExpInt _) = return TypeInt
    ctExpConstant (ExpBool _) = return TypeBool
    ctExpConstant (ExpString _) = return TypeString


canAExp :: Operation -> Exp -> TES TypeSpec
canAExp op e = do
    ts <- ctExp e
    canAF op ts
-- That was intended to be here, along with a nice function for printing expression trees...
-- That would be an actual help while debugging the program, but it's not the case right now.
--} `catchError` ((\e err -> throwError $ show err ++ "In expression " ++ show e) e)


canAExp2 :: Operation -> Exp -> Exp -> TES TypeSpec
canAExp2 op e1 e2 = do
    ts1 <- ctExp e1
    ts2 <- ctExp e2
    canAF2 op ts1 ts2
--} `catchError` ((\e1 e2 err -> throwError $ show err ++ "In expressions\n" ++ show e1 ++ "\n" ++ show e2) e1 e2)


canAF :: Operation -> TypeSpec -> TES TypeSpec
canAF UInc TypeInt = return TypeInt
canAF UDec TypeInt = return TypeInt
canAF PInc TypeInt = return TypeInt
canAF PDec TypeInt = return TypeInt
canAF UPos TypeInt = return TypeInt
canAF UNeg TypeInt = return TypeInt
canAF UNot TypeBool = return TypeBool
canAF UFlp TypeInt = return TypeInt
canAF op t = throwError $ "Cannot use operation '" ++ show op ++ "' on type '" ++ showTS t ++ "'"


canAF2 :: Operation -> TypeSpec -> TypeSpec -> TES TypeSpec
canAF2 As t1 t2 | t1 == t2 = return t1
canAF2 AsMul t1 t2 = canAF2 Mul t1 t2
canAF2 AsDiv t1 t2 = canAF2 Div t1 t2
canAF2 AsMod t1 t2 = canAF2 Mod t1 t2
canAF2 AsAdd t1 t2 = canAF2 Add t1 t2
canAF2 AsSub t1 t2 = canAF2 Sub t1 t2
canAF2 AsAnd t1 t2 = canAF2 LogAnd t1 t2
canAF2 AsOr t1 t2 = canAF2 LogOr t1 t2
canAF2 LogOr TypeBool TypeBool = return TypeBool
canAF2 LogXor TypeBool TypeBool = return TypeBool
canAF2 LogAnd TypeBool TypeBool = return TypeBool
canAF2 Ieq TypeBool TypeBool = return TypeBool
canAF2 Ieq TypeInt TypeInt = return TypeBool
canAF2 Ieq TypeString TypeString = return TypeBool
canAF2 Neq t1 t2 = canAF2 Ieq t1 t2
canAF2 Lt TypeInt TypeInt = return TypeBool
canAF2 Gt TypeInt TypeInt = return TypeBool
canAF2 Le TypeInt TypeInt = return TypeBool
canAF2 Ge TypeInt TypeInt = return TypeBool
canAF2 Add TypeInt TypeInt = return TypeInt
canAF2 Add TypeString TypeString = return TypeString
canAF2 Sub TypeInt TypeInt = return TypeInt
canAF2 Mul TypeInt TypeInt = return TypeInt
canAF2 Div TypeInt TypeInt = return TypeInt
canAF2 Mod TypeInt TypeInt = return TypeInt
canAF2 op t1 t2 = throwError $ "Cannot use operation " ++ show op ++ " on types " ++
                               showTS t1 ++ ", " ++ showTS t2


makeEmptyExp :: Exp
makeEmptyExp = ExpConstant $ ExpBool ConstantTrue


-- Statements


ctStmt :: Stmt -> TES ()
ctStmt (StmtExp e) = do
    _ <- ctExp e
    return ()
--    } `catchError` ((\e err -> throwError (show err ++ "\nIn expression " ++ show e)) e)
ctStmt (StmtCompound stmt) = ctCompoundStmt stmt
ctStmt (StmtSelection stmt) = ctSelectionStmt stmt
ctStmt (StmtIteration stmt) = ctIterationStmt stmt
ctStmt (StmtReturn e) = ctReturnStmt e
ctStmt (StmtDecl d) = ctDecl d


ctCompoundStmt :: CompoundStmt -> TES ()
ctCompoundStmt (StmtCompoundEmpty) = return ()
ctCompoundStmt (StmtCompoundList stmts) = do
    (env, tstore, fenv, _) <- lift get
    mapM_ ctStmt stmts
    (_, _, _, rtype) <- lift get
    lift $ put (env, tstore, fenv, rtype)
    return ()

ctSelectionStmt :: SelectionStmt -> TES ()
ctSelectionStmt (StmtIf e stmt) = do
    et <- ctExp e
    if et == TypeBool
      then return ()
      else throwError $ "Non-boolean value in 'if' condition: " ++ show e
    ctCompoundStmt stmt
ctSelectionStmt (StmtIfElse e stmt1 stmt2) = do
    _ <- ctSelectionStmt (StmtIf e stmt1)
    ctCompoundStmt stmt2
ctIterationStmt :: IterationStmt -> TES ()
ctIterationStmt (StmtWhile e stmt) = do
    et <- ctExp e
    if et == TypeBool
      then return ()
      else throwError $ "Non-boolean value in condition of 'while' loop: " ++ show e
    ctCompoundStmt stmt
ctIterationStmt (StmtFor1 pre cond post stmt) = do
    _ <- ctExp pre
    _ <- ctExp post
    et <- ctExp cond
    if et == TypeBool
      then return ()
      else throwError $ "Non-boolean value in condition of 'for' loop: " ++ show cond
    ctCompoundStmt stmt
ctIterationStmt (StmtFor2 pre cond stmt) =
    ctIterationStmt (StmtFor1 pre cond makeEmptyExp stmt)
ctIterationStmt (StmtFor3 pre post stmt) =
    ctIterationStmt (StmtFor1 pre makeEmptyExp post stmt)
ctIterationStmt (StmtFor4 cond post stmt) =
    ctIterationStmt (StmtFor1 makeEmptyExp cond post stmt)
ctIterationStmt (StmtFor5 post stmt) =
    ctIterationStmt (StmtFor1 makeEmptyExp makeEmptyExp post stmt)
ctIterationStmt (StmtFor6 cond stmt) =
    ctIterationStmt (StmtFor1 makeEmptyExp cond makeEmptyExp stmt)
ctIterationStmt (StmtFor7 pre stmt) =
    ctIterationStmt (StmtFor1 pre makeEmptyExp makeEmptyExp stmt)
ctIterationStmt (StmtFor8 stmt) =
    ctIterationStmt (StmtFor1 makeEmptyExp makeEmptyExp makeEmptyExp stmt)


ctReturnStmt :: Exp -> TES ()
ctReturnStmt e = do
    rtype <- ctExp e
    (env, tstore, fenv, _) <- lift get
    lift $ put $ (env, tstore, fenv, rtype)
    return ()



checkTypes :: TranslationUnit -> TES ()
checkTypes = ctTranslationUnit
