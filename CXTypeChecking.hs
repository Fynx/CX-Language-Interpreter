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


-- Statements

ctCompoundStmt :: CompoundStmt -> TES ()
ctCompoundStmt cs = return ()


ctFunction :: (Ident, Loc) -> TES ()
ctFunction (id, loc) = do
    (env, tstore, fargs, _) <- lift get
    liftIO $ putStrLn $ show id
    case Map.lookup loc fargs of
      Nothing -> do
        liftIO $ putStrLn $ "Not a function: " ++ show id
        return () -- Not a function
      Just (ts, args, stmt) -> do
        liftIO $ putStrLn $ "Check function " ++ show id ++ " with type " ++ showTS ts ++
                            " and args " ++ show args ++ "\n"
        _ <- ctCompoundStmt stmt
        (_, _, _, rtype) <- lift get
        if rtype == ts
          then return ()
          else throwError $ "Invalid return value of function " ++ show id ++
                            "\n  Expected type: " ++ showTS ts ++ "\n  Actual type:   " ++
                            showTS rtype


showTS :: TypeSpec -> String
showTS TypeInt    = "Int"
showTS TypeVoid   = "Void"
showTS TypeString = "String"
showTS TypeBool   = "Bool"


checkTypes :: TES ()
checkTypes = do
    (env, tstore, fargs, rtype) <- lift get
    liftIO $ putStrLn "\nEnv:"
    liftIO $ print env
    liftIO $ putStrLn ""
    mapM_ ctFunction (Map.assocs env)
    return ()
