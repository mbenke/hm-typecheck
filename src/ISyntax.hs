module ISyntax where
import Types

type Name = String
data Expr
    = ELam [Arg] Expr
    | ELet Name Expr Expr
--    | ERec [Decl] Expr
    | EApp Expr Expr
    | EVar Name
    | EInt Integer
  deriving (Eq, Show)

-- data Arg = UArg LIdent
--  deriving (Eq, Ord, Show, Read)

type Arg = Name


data Decl
    = TypeDecl Type [ConAlt]
    | ValDecl Name (Qual Type)
    | ValBind Name [Arg] Expr
    | InstDecl (Qual Pred)
  deriving (Eq, Show)

data ConAlt = ConAlt Name [Type]
  deriving (Eq, Show)


data Prog = Prog [Decl]

showExpr :: Expr -> String
showExpr (ELam vs e) = concat ["\\", unwords vs, " -> ", showExpr e] where
showExpr (EVar v) = v
showExpr (EApp e1 e2) = concat [showExpr e1, "(", showExpr e2, ")"]
showExpr (ELet x e1 e2) = concat ["let ",  x, " = ", showExpr e1, " in ", showExpr e2]
showExpr e = show e
