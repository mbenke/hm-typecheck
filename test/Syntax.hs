{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Syntax(
  Prog(..), Expr(..), Arg(..), Decl(..), Name,
  CType(..), CPred(..), QPred(..), QType(..),
  TyDeRhs(..), ConAlt(..), LIdent(..), UIdent(..), Methods(..),
  name, expr, prog, decl, showExpr,
  BNFC.Print(..),
  module Language.LBNF.Runtime
             ) where
import qualified Language.LBNF.Compiletime as BNFC
import Language.LBNF.Compiletime
import Language.LBNF.Runtime(printTree)
import Language.LBNF(lbnf, bnfc)


-- Remember to kep this in sync with Fun.cf
bnfc [lbnf|
token UIdent (upper (letter | digit | '_')*) ;
token LIdent (lower (letter | digit | '_')*) ;

Prog . Prog ::= [Decl];

EBlock . Expr ::= "{" [Stmt] "}";
ETyped . Expr ::= Expr1 ":" CType;
ELam . Expr ::= "\\" [Arg] "->" Expr ;
ELet . Expr ::= "let" LIdent "=" Expr "in" Expr;
EApp . Expr1 ::= Expr1 Expr2 ;
EVar . Expr2 ::= LIdent;
ECon . Expr2 ::= UIdent;
EInt . Expr2 ::= Integer;
coercions Expr 2;

UArg . Arg ::= LIdent;
separator Arg "";

SExpr   . Stmt ::= Expr;
SAssign . Stmt ::= Expr "=" Expr;
SAlloc  . Stmt ::= "let" LIdent ":" CType;
SInit   . Stmt ::= "let" LIdent "=" Expr;
separator Stmt ";";

TypeDecl. Decl ::= "type" CType TyDeRhs;
ValDecl. Decl ::= LIdent ":" QType;
ValBind. Decl ::= LIdent [Arg] "=" Expr;
InstDecl. Decl ::= "instance" QPred;
ClsDecl . Decl ::= "class" CPred Methods;
separator Decl ";";

NoMethods   . Methods ::= {- empty -} ;
SomeMethods . Methods ::= "{" [Decl] "}";

-- Cannot use Ctxt here due to conflicts
T0Qual . QType ::=  CType;
T1Qual . QType ::=  CPred "=>" CType;
TNQual . QType ::=  "(" [CPred] ")" "=>" CType;

I0Qual . QPred ::=  CPred;
I1Qual . QPred ::=  CPred "=>" CPred;
INQual . QPred ::=  "(" [CPred] ")" "=>" CPred;


-- Ctxt0 . Ctxt ::= {- empty -};
-- CtxtN . Ctxt ::= "(" [CPred] ")" "=>";

EmptyTyDeRhs . TyDeRhs ::= ;
ConAlts . TyDeRhs ::= "=" [ConAlt];

ConAlt0 . ConAlt ::= UIdent;
ConAltN . ConAlt ::= UIdent "[" [CType] "]" ;

separator ConAlt "|";

PSingle . CPred ::= CType ":" UIdent ;
PMulti  . CPred ::= CType ":" UIdent "[" [CType] "]" ;
separator CPred ",";

-- define tarr t1 t2 = TCon (Ident "->") [t1,t2] ;

CTArr . CType ::= CType1 "->" CType;
CTCon . CType1 ::= UIdent "[" [CType] "]";
CTCon0 . CType2 ::= UIdent;
CTVar . CType2 ::= LIdent;
coercions CType 2;

separator CType ",";

comment "//" ;
comment "/*" "*/" ;
|]

type Name = String

class HasName i where
    name :: i -> Name

-- instance HasName Ident where
--     name (Ident s) = s

instance HasName UIdent where
    name (UIdent s) = s

instance HasName LIdent where
    name (LIdent s) = s

showExpr :: Expr -> String
showExpr = printTree
