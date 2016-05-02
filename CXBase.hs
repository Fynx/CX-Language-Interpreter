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

type Env = Map.Map Ident DataType


defaultValue :: TypeSpec -> DataType
defaultValue TypeBool   = (TBool False)
defaultValue TypeInt    = (TInt 0)
defaultValue TypeString = (TString "")
