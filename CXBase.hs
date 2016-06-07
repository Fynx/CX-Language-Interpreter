module CXBase where


import qualified Data.Map as Map
import Data.Bool
import Data.Bits

import AbsCX


type Loc = Integer

data DataType = 
      TVoid
    | TBool   Bool
    | TInt    Integer
    | TString String
    | TRef    Loc
    | TFun    Ident
    deriving (Show)


elogxor :: DataType -> DataType -> DataType
elogxor (TBool v1) (TBool v2) = TBool $ (v1 && v2) || ((not v1) && (not v2))

elogor :: DataType -> DataType -> DataType
elogor (TBool v1) (TBool v2) = TBool (v1 || v2)

elogand :: DataType -> DataType -> DataType
elogand (TBool v1) (TBool v2) = TBool (v1 && v2)

eeq :: DataType -> DataType -> DataType
eeq (TBool v1) (TBool v2) = TBool (v1 && v2)
eeq (TInt v1) (TInt v2) = TBool (v1 == v2)
eeq (TString v1) (TString v2) = TBool (v1 == v2)

eneq :: DataType -> DataType -> DataType
eneq t1 t2 = eeq t1 t2

elt :: DataType -> DataType -> DataType
elt (TInt v1) (TInt v2) = TBool (v1 < v2)

egt :: DataType -> DataType -> DataType
egt (TInt v1) (TInt v2) = TBool (v1 > v2)

ele :: DataType -> DataType -> DataType
ele (TInt v1) (TInt v2) = TBool (v1 <= v2)

ege :: DataType -> DataType -> DataType
ege (TInt v1) (TInt v2) = TBool (v1 >= v2)

eadd :: DataType -> DataType -> DataType
eadd (TInt v1) (TInt v2) = TInt (v1 + v2)
eadd (TString v1) (TString v2) = TString (v1 ++ v2)

esub :: DataType -> DataType -> DataType
esub (TInt v1) (TInt v2) = TInt (v1 - v2)

emul :: DataType -> DataType -> DataType
emul (TInt v1) (TInt v2) = TInt (v1 * v2)

ediv :: DataType -> DataType -> DataType
ediv (TInt v1) (TInt v2) = TInt (div v1 v2)

emod :: DataType -> DataType -> DataType
emod (TInt v1) (TInt v2) = TInt (mod v1 v2)

einc :: DataType -> DataType
einc (TInt v) = TInt (v + 1)

edec :: DataType -> DataType
edec (TInt v) = TInt (v - 1)


upos :: DataType -> DataType
upos (TInt v) = TInt v

uneg :: DataType -> DataType
uneg (TInt v) = TInt $ -v

unot :: DataType -> DataType
unot (TBool v) = TBool $ not v

uflp :: DataType -> DataType
uflp (TInt v) = TInt $ complement v


defaultValue :: TypeSpec -> DataType
defaultValue TypeBool   = (TBool False)
defaultValue TypeInt    = (TInt 0)
defaultValue TypeString = (TString "")


type Env = Map.Map Ident Loc
type Store = Map.Map Loc DataType
--TODO change to FunSpec
type FunArgs = Map.Map Loc (TypeSpec, [Arg], CompoundStmt)
--TODO change to RetVal
type Local = DataType
