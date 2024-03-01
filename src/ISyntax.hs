module ISyntax where
import Data.List(union, intersect, nub, (\\), intercalate)
import Types

type Name = String
data Expr
    = ELam [Arg] Expr
    | ELet Name Expr Expr
    | EApp Expr Expr
    | EVar Name
    | ECon Name
    | EInt Integer
    | EBlock [Stmt String]  -- desugared statements annotated with their source form
  deriving Eq

-- data Arg = UArg LIdent
--  deriving (Eq, Ord, Show, Read)

type Arg = Name

data Stmt ann             -- ann - annotation (e.g. stmt before desugar)
    = SExpr ann Expr 
--    | SAssign ann Expr Expr
    | SAlloc ann Name Type 
    | SInit ann Name Expr
  deriving (Eq)

data Decl
    = TypeDecl Type [ConAlt]
    | ValDecl Name (Qual Type)
    | ValBind Name [Arg] Expr
    | InstDecl (Qual Pred)
    | ClsDecl Pred [Decl]
    | Pragma String
  deriving (Eq, Show)

data ConAlt = ConAlt Name [Type]
  deriving (Eq, Show)


data Prog = Prog [Decl]

instance Show Expr where
  showsPrec d (EInt n) = showsPrec 10 n
  showsPrec d (EVar n) = showString n
  showsPrec d (ECon n) = showString n
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec (ap_prec) e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
         where ap_prec = 10

  showsPrec d (ELam vs e) = showParen (d > lam_prec) $
             showString "\\" . showString(unwords vs) . showString " -> " .
             showsPrec (lam_prec) e
         where lam_prec = 1

  showsPrec d (ELet x e1 e2) = showParen (d > let_prec) $
             showString "let " . showString x . showString "= " . showsPrec 0 e1 .
             showsPrec let_prec e2
         where let_prec = 2

  showsPrec d (EBlock stmts) = showString "{ " .
                               showString ( intercalate "; " (map show stmts)) .
                               showString "}"
showExpr :: Expr -> String
-- showExpr (ELam vs e) = concat ["\\", unwords vs, " -> ", showExpr e] where
-- showExpr (EVar v) = v
-- showExpr (ECon v) = v
-- showExpr (EApp (EVar f) arg) = concat [f, "(", showExpr arg, ")"]
-- showExpr (EApp  e1 e2) = concat ["(", showExpr e1, ")", "(", showExpr e2, ")"]
-- showExpr (ELet x e1 e2) = concat ["let ",  x, " = ", showExpr e1, " in ", showExpr e2]
-- showExpr (EBlock stmts) = intercalate "; " (map showStmt stmts)
showExpr e = show e

instance Show (Stmt ann) where
    show (SExpr _ e) = showExpr e
    show (SAlloc _ x t) = concat ["let ",  x, " : ", show t]

class HasFreeVars a where
    freeVars :: a -> [Name]

instance HasFreeVars Expr where
    freeVars (EVar n) = [n]
    freeVars (EApp e1 e2) = union (freeVars e1) (freeVars e2)
    freeVars (ELam vs e) = freeVars e \\ vs
    freeVars _ = [] -- FIXME
    -- freeVars e = error("freeVars unimplemented for: " ++show e)
