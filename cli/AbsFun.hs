-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Fun.

module AbsFun where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Prog = Prog [Decl]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EBlock [Stmt]
    | ELam [Arg] Expr
    | ELet LIdent Expr Expr
    | ERec [Decl] Expr
    | EApp Expr Expr
    | EMet Expr Expr
    | EIdx Expr Expr
    | EStar LIdent
    | EVar LIdent
    | ECon UIdent
    | EInt Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = UArg LIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = SExpr Expr
    | SAssign Expr Expr
    | SAlloc LIdent CType
    | SInit LIdent Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl
    = TypeDecl CType TyDeRhs
    | ValDecl LIdent QType
    | ValBind LIdent [Arg] Expr
    | InstDecl QPred
    | ClsDecl CPred Methods
    | Pragma LIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Methods = NoMethods | SomeMethods [Decl]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data QType
    = T0Qual CType | T1Qual CPred CType | TNQual [CPred] CType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data QPred
    = I0Qual CPred | I1Qual CPred CPred | INQual [CPred] CPred
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TyDeRhs = EmptyTyDeRhs | ConAlts [ConAlt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConAlt = ConAlt0 UIdent | ConAltN UIdent [CType]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CPred = PSingle CType UIdent | PMulti CType UIdent [CType]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CType
    = CTArr CType CType
    | CTCon UIdent [CType]
    | CTCon0 UIdent
    | CTVar LIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype UIdent = UIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype LIdent = LIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

