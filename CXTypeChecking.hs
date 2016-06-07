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

type TypeCont = (Env, TStore, FunArgs, RetType)

type TES a = ExceptT String (StateT TypeCont (IO)) a


-- Declarations

ctDecl :: Decl -> TES ()
ctDecl (DeclDefault ts ids) = return ()
ctDecl (DeclDefine ts id e) = do
    et <- ctExp e
    if ts == et
      then return ()
      else throwError $ "Attempting to assign invalid value to variable " ++ showId id


-- Expressions

ctExp :: Exp -> TES TypeSpec
ctExp (ExpAssign e1 op e2) = ctExpAssign op e1 e2
ctExp (ExpCondition cond e1 e2) = do
    ct <- ctExp cond
    if ct == TypeBool
      then return TypeBool
      else throwError $ "Non-boolean value in condition: " ++ show cond
ctExp (ExpLogOr e1 e2) = ctExpLogOr e1 e2
ctExp (ExpEq e1 e2) = ctExpEq e1 e2
ctExp (ExpNeq e1 e2) = ctExpNeq e1 e2
ctExp (ExpLt e1 e2) = ctExpLt e1 e2
ctExp (ExpGt e1 e2) = ctExpGt e1 e2
ctExp (ExpLe e1 e2) = ctExpLe e1 e2
ctExp (ExpGe e1 e2) = ctExpGe e1 e2
ctExp (ExpAdd e1 e2) = ctExpAdd e1 e2
ctExp (ExpSub e1 e2) = ctExpSub e1 e2
ctExp (ExpMul e1 e2) = ctExpMul e1 e2
ctExp (ExpDiv e1 e2) = ctExpDiv e1 e2
ctExp (ExpMod e1 e2) = ctExpMod e1 e2
ctExp (ExpUnaryInc e) = ctExpUnaryInc e
ctExp (ExpUnaryDec e) = ctExpUnaryDec e
ctExp (ExpPostInc uop e) = ctExpPostInc e
ctExp (ExpFuncP e) = ctExp (ExpFuncPArgs e [])
ctExp (ExpFuncP e) = ctExp (ExpFuncPArgs e [])
ctExp (ExpFuncPArgs (ExpConstant (ExpId id)) args) = do
    (env, _, fargs, _) <- lift get
    case Map.lookup id env of
      Nothing -> throwError $ "Cannot find a function '" ++ showId id ++ "'"
      Just loc -> do
        case Map.lookup loc fargs of
          Nothing -> throwError $ "Internal error: cannot find a function '" ++ showId id ++ "'"
          Just (ts, rargs, _) -> do
            argts <- mapM ctExp args
            let rargts = map extractType rargs
            if compareArrays argts rargts
              then return ts
              else throwError $ "Arguments of function '" ++ showId id ++ "' do not match.\n" ++
                                "  Expected types: " ++ show rargts ++ "\n  Actual types: " ++ show argts
            where
                extractType (ArgVal t _) = t
                extractType (ArgRef t _) = t
                compareArrays [] [] = True
                compareArrays l [] = False
                compareArrays [] l = False
                compareArrays (x:xs) (y:ys)
                    | x == y    = compareArrays xs ys
                    | otherwise = False
ctExp (ExpFuncPArgs e _) = throwError $ "Expressions with functions hidden inside not implemented: " ++ show e
ctExp (ExpConstant c) = return TypeVoid


ctExpAssign op e1 e2 = return TypeVoid

ctExpLogOr e1 e2 = return TypeVoid
ctExpEq e1 e2 = return TypeVoid
ctExpNeq e1 e2 = return TypeVoid
ctExpLt e1 e2 = return TypeVoid
ctExpGt e1 e2 = return TypeVoid
ctExpLe e1 e2 = return TypeVoid
ctExpGe e1 e2 = return TypeVoid
ctExpAdd e1 e2 = return TypeVoid
ctExpSub e1 e2 = return TypeVoid
ctExpMul e1 e2 = return TypeVoid
ctExpDiv e1 e2 = return TypeVoid
ctExpMod e1 e2 = return TypeVoid
ctExpUnaryInc e = return TypeVoid
ctExpUnaryDec e = return TypeVoid
ctExpPostInc e = return TypeVoid


makeEmptyExp :: Exp
makeEmptyExp = ExpConstant $ ExpBool ConstantTrue


-- Statements


--TODO catch errors as they go out

ctStmt :: Stmt -> TES ()
ctStmt (StmtExp e) = do
    _ <- ctExp e
    return ()
ctStmt (StmtCompound stmt) = ctCompoundStmt stmt
ctStmt (StmtSelection stmt) = ctSelectionStmt stmt
ctStmt (StmtIteration stmt) = ctIterationStmt stmt
ctStmt (StmtReturn e) = ctReturnStmt e
ctStmt (StmtDecl d) = ctDecl d


ctCompoundStmt :: CompoundStmt -> TES ()
ctCompoundStmt (StmtCompoundEmpty) = return ()
ctCompoundStmt (StmtCompoundList stmts) = do
    mapM_ ctStmt stmts
    return ()

ctSelectionStmt :: SelectionStmt -> TES ()
ctSelectionStmt (StmtIf e stmt) = do
    et <- ctExp e
    if et == TypeBool
      then return () --TODO make it pretty
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
    (env, tstore, fargs, _) <- lift get
    lift $ put $ (env, tstore, fargs, rtype)
    return ()


-- Functions


ctFunction :: (Ident, Loc) -> TES ()
ctFunction (id, loc) = do
    (env, tstore, fargs, _) <- lift get
    case Map.lookup loc fargs of
      Nothing -> do
        liftIO $ putStrLn $ "Not a function: " ++ showId id
        return () -- Not a function
      Just (ts, args, stmt) -> do
        liftIO $ putStrLn $ "Check function " ++ showId id ++ " with type " ++ showTS ts ++
                            " and args " ++ show args ++ "\n"
        _ <- ctCompoundStmt stmt
        (_, _, _, rtype) <- lift get
        lift $ put $ (env, tstore, fargs, TypeVoid)
        if rtype == ts
          then return ()
          else throwError $ "Invalid return value of function " ++ showId id ++
                            "\n  Expected type: " ++ showTS ts ++ "\n  Actual type:   " ++
                            showTS rtype


showTS :: TypeSpec -> String
showTS TypeInt    = "Int"
showTS TypeVoid   = "Void"
showTS TypeString = "String"
showTS TypeBool   = "Bool"

--TODO move somewhere else
showId :: Ident -> String
showId (Ident s) = s


checkTypes :: TES ()
checkTypes = do
    (env, tstore, fargs, rtype) <- lift get
    liftIO $ putStrLn "\nEnv:"
    liftIO $ print env
    liftIO $ putStrLn ""
    mapM_ ctFunction (Map.assocs env)
    return ()
