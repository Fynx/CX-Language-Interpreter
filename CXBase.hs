module CXBase where


import qualified Data.Map as Map

import AbsCX


type Loc = Integer

data DataType = 
      TVoid
    | TBool   Bool
    | TInt    Integer
    | TString String
    | TRef    DataType Loc
    deriving (Show)


defaultValue :: TypeSpec -> DataType
defaultValue TypeBool   = (TBool False)
defaultValue TypeInt    = (TInt 0)
defaultValue TypeString = (TString "")


type Env = Map.Map Ident Loc
type Store = Map.Map Loc DataType
type FunArgs = Map.Map Loc [DataType]
type Cont = (Env, Store, FunArgs)

emptyCont :: Cont
emptyCont = (Map.empty, Map.empty, Map.empty)
