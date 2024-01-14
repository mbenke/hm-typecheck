{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module MLExpr(
  Prog(..), Expr(..), Arg(..), Decl(..), CType(..), Name,
  name, expr, prog, decl, showExpr,
  BNFC.Print(..),
  module Language.LBNF.Runtime
             ) where
import qualified Language.LBNF.Compiletime as BNFC
import Language.LBNF.Compiletime
import Language.LBNF.Runtime(printTree)
import Language.LBNF(lbnf, bnfc)


bnfc [lbnf|
Prog . Prog ::= [Decl];
ELam . Expr ::= "\\" [Arg] "->" Expr ;
ELet . Expr ::= "let" Ident "=" Expr "in" Expr;
ERec . Expr ::= "letrec" [Decl] "in" Expr;
EApp . Expr1 ::= Expr1 Expr2 ;
EVar . Expr2 ::= Ident;
EInt . Expr2 ::= Integer;
coercions Expr 2;

UArg . Arg ::= Ident;
separator Arg "";

ValDecl. Decl ::= Ident "::" CType;
ValBind. Decl ::= Ident [Arg] "=" Expr;
I0Qual . Decl ::= "instance" CPred;
I1Qual . Decl ::= "instance" CPred "=>" CPred;
separator Decl ";";


PSingle . CPred ::= Ident CType ;
PMulti  . CPred ::= Ident "[" [CType] "]" CType;

-- define tarr t1 t2 = TCon (Ident "->") [t1,t2] ;

CTArr . CType ::= CType1 "->" CType1;
CTCon . CType1 ::= Ident "[" [CType] "]";
CTVar . CType2 ::= Ident;
coercions CType 2;

separator CType ",";

comment "//" ;
comment "/*" "*/" ;
|]

type Name = String
name :: Ident -> Name
name (Ident s) = s

showExpr :: Expr -> String
showExpr = printTree
