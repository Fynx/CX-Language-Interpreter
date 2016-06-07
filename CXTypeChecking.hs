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
ctExp (ExpAssign e1 op e2) = return TypeVoid
ctExp (ExpCondition cond e1 e2) = return TypeVoid
ctExp (ExpLogOr e1 e2) = return TypeVoid
ctExp (ExpEq e1 e2) = return TypeVoid
ctExp (ExpNeq e1 e2) = return TypeVoid
ctExp (ExpLt e1 e2) = return TypeVoid
ctExp (ExpGt e1 e2) = return TypeVoid
ctExp (ExpLe e1 e2) = return TypeVoid
ctExp (ExpGe e1 e2) = return TypeVoid
ctExp (ExpAdd e1 e2) = return TypeVoid
ctExp (ExpSub e1 e2) = return TypeVoid
ctExp (ExpMul e1 e2) = return TypeVoid
ctExp (ExpDiv e1 e2) = return TypeVoid
ctExp (ExpMod e1 e2) = return TypeVoid
ctExp (ExpUnaryInc e) = return TypeVoid
ctExp (ExpUnaryDec e) = return TypeVoid
ctExp (ExpPostInc uop e) = return TypeVoid
ctExp (ExpFuncP e) = return TypeVoid
ctExp (ExpFuncPArgs e args) = return TypeVoid
ctExp (ExpConstant c) = return TypeVoid


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
    case et of
      TypeBool -> return () --TODO make it pretty
      otherwise -> throwError $ "Non-boolean value in 'if' condition: " ++ show e
    ctCompoundStmt stmt
ctSelectionStmt (StmtIfElse e stmt1 stmt2) = do
    _ <- ctSelectionStmt (StmtIf e stmt1)
    ctCompoundStmt stmt2
ctIterationStmt :: IterationStmt -> TES ()
ctIterationStmt (StmtWhile e stmt) = do
    et <- ctExp e
    case et of
      TypeBool -> return ()
      otherwise -> throwError $ "Non-boolean value in condition of 'while' loop: " ++ show e
    ctCompoundStmt stmt
ctIterationStmt (StmtFor1 pre cond post stmt) = do
    _ <- ctExp pre
    _ <- ctExp post
    et <- ctExp cond
    case et of
      TypeBool -> return ()
      otherwise -> throwError $ "Non-boolean value in condition of 'for' loop: " ++ show cond
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
