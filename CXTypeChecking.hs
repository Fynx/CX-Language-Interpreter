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

type TypeCont = (Env, FunArgs)

emptyTypeCont :: TypeCont
emptyTypeCont = (Map.empty, Map.empty)


type TES a = ExceptT String (StateT TypeCont (IO)) a


checkTypes :: (Env, FunArgs) -> TES ()
checkTypes (env, fargs) = do
    liftIO $ putStrLn "Env:"
    liftIO $ print env
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Fargs:"
    liftIO $ print fargs
    liftIO $ putStrLn ""
    return ()
