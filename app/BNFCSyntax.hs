{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module BNFCSyntax where
import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc)

bnfc [lbnf|

ELam . Expr0 ::= "\\" Ident "->" Expr0 ;
EApp . Expr1 ::= Expr1 Expr2 ;
EVar . Expr9 ::= Ident;

coercions Expr 10;
|]    
