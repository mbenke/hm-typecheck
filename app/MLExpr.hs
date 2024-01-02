{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module MLExpr where
import Language.LBNF.Compiletime
import Language.LBNF.Runtime    
import Language.LBNF(lbnf, bnfc)

bnfc [lbnf|
ELam . Expr ::= "\\" [Arg] "->" Expr ;
ELet . Expr ::= "let" Ident "=" Expr "in" Expr;
ERec . Expr ::= "letrec" [Def] "in" Expr;
EApp . Expr1 ::= Expr1 Expr2 ;
EVar . Expr2 ::= Ident;
EInt . Expr2 ::= Integer;
coercions Expr 2;

UArg . Arg ::= Ident;
separator Arg "";

ValDec . Def ::= Ident "=" Expr; 
separator Def ";"
|]    

type Name = String
name :: Ident -> Name
name (Ident s) = s

showExpr :: Expr -> String
showExpr = printTree
