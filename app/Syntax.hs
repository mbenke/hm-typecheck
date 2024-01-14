{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Syntax(
  Prog(..), Expr(..), Arg(..), Decl(..), CType(..), CPred(..), Name,
  name, expr, prog, decl, showExpr,
  BNFC.Print(..),
  module Language.LBNF.Runtime
             ) where
import qualified Language.LBNF.Compiletime as BNFC
import Language.LBNF.Compiletime
import Language.LBNF.Runtime(printTree)
import Language.LBNF(lbnf, bnfc)


bnfc [lbnf|
token UIdent (upper (letter | digit | '_')*) ;
token LIdent (lower (letter | digit | '_')*) ;

Prog . Prog ::= [Decl];
ELam . Expr ::= "\\" [Arg] "->" Expr ;
ELet . Expr ::= "let" LIdent "=" Expr "in" Expr;
ERec . Expr ::= "letrec" [Decl] "in" Expr;
EApp . Expr1 ::= Expr1 Expr2 ;
EVar . Expr2 ::= LIdent;
EInt . Expr2 ::= Integer;
coercions Expr 2;

UArg . Arg ::= LIdent;
separator Arg "";

TypeDecl. Decl ::= "type" CType;
ValDecl. Decl ::= LIdent "::" CType;
ValBind. Decl ::= LIdent [Arg] "=" Expr;
I0Qual . Decl ::= "instance" CPred;
I1Qual . Decl ::= "instance" CPred "=>" CPred;
separator Decl ";";


PSingle . CPred ::= UIdent CType ;
PMulti  . CPred ::= UIdent "[" [CType] "]" CType;

-- define tarr t1 t2 = TCon (Ident "->") [t1,t2] ;

CTArr . CType ::= CType1 "->" CType1;
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
