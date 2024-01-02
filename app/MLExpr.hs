{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module MLExpr where
import Language.LBNF.Compiletime
import Language.LBNF.Runtime    
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

separator Decl ";";

-- define tarr t1 t2 = TCon (Ident "->") [t1,t2] ;

CTArr . CType ::= CType "->" CType1;
CTCon . CType1 ::= Ident "(" [CType] ")";
CTVar . CType2 ::= Ident;
coercions CType 2;

separator CType ",";
|]    

type Name = String
name :: Ident -> Name
name (Ident s) = s

showExpr :: Expr -> String
showExpr = printTree
