{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Language.Fun.ISyntax
( Name, Expr
, ExpX( .., ELam, ELet, EApp, EVapp, EVar, ECon
       , EInt, EBlock, ETyped, ECase)
, XLam, XLet, XApp, XVapp, XVar, XCon
, XInt, XBlock, XTyped, XCase, XExp
, Arg(..), argName
, CaseAltX(..), CaseAlt
, Stmt(..)
, DeclX(..), Decl
, BindX(..), Bind
, ConAlt(..)
, ProgX(..), Prog
, ToStr(..), HasTypes(..), HasFreeVars(..)
, showExpr, showDecl
) where
import Data.List(union, intersect, nub, (\\), intercalate)
import Language.Fun.Types
import Language.Fun.Constraints(HasTypes(..))
import Language.Fun.Phase

type Name = String

data ExpX x
    = ELamX (XLam x) [Arg] (ExpX x)         -- function \args -> expr
    | ELetX (XLet x) Name (ExpX x) (ExpX x) -- local definition: let name = expr1 in expr2
    | EAppX (XApp x) (ExpX x) (ExpX x)         -- function call: f(arg)
    | EVappX (XApp x) (ExpX x) [(ExpX x)]      -- vector application: f(args)
    | EVarX  (XVar x) Name              -- variable
    | EConX (XCon x) Name              -- value constructor
    | EIntX (XInt x) Integer           -- integer literal
    | EBlockX (XBlock x) [Stmt String]   -- desugared statements annotated with their source form
    | ETypedX (XTyped x) (ExpX x) Type
    | ECaseX (XCase x) (ExpX x) [CaseAlt]
    | ExpX (XExp x)

type family XLam x
type family XLet x
type family XApp x
type family XVapp x
type family XVar x
type family XCon x
type family XInt x
type family XBlock x
type family XTyped x
type family XCase x
type family XExp x

type Expr = ExpX FunUD

type instance XLam FunUD = NoExtField
type instance XLet FunUD = NoExtField
type instance XApp FunUD = NoExtField
type instance XVapp FunUD = NoExtField
type instance XVar FunUD = NoExtField
type instance XCon FunUD = NoExtField
type instance XInt FunUD = NoExtField
type instance XBlock FunUD = NoExtField
type instance XTyped FunUD = NoExtField
type instance XCase FunUD = NoExtField
type instance XExp FunUD = DataConCantHappen

pattern ELam :: [Arg] -> Expr -> Expr
pattern ELam args e <- ELamX _ args e
  where ELam args e = ELamX NoExtField args e

pattern ELet :: Name -> Expr -> Expr -> Expr
pattern ELet x e1 e2 <- ELetX _ x e1 e2
  where ELet x e1 e2 = ELetX NoExtField x e1 e2

pattern EApp :: Expr -> Expr -> Expr
pattern EApp e1 e2 <- EAppX _ e1 e2
  where EApp e1 e2 = EAppX NoExtField e1 e2

pattern EVapp :: Expr -> [Expr] -> Expr
pattern EVapp e es <- EVappX _ e es
  where EVapp e es = EVappX NoExtField e es

pattern EVar :: Name -> Expr
pattern EVar n <- EVarX _ n
  where EVar n = EVarX NoExtField n

pattern ECon :: Name -> Expr
pattern ECon n <- EConX _ n
  where ECon n = EConX NoExtField n

pattern EInt :: Integer -> Expr
pattern EInt n <- EIntX _ n
  where EInt n = EIntX NoExtField n

pattern EBlock :: [Stmt String] -> Expr
pattern EBlock stmts <- EBlockX _ stmts
  where EBlock stmts = EBlockX NoExtField stmts

pattern ETyped :: Expr -> Type -> Expr
pattern ETyped e t <- ETypedX _ e t
  where ETyped e t = ETypedX NoExtField e t

pattern ECase :: Expr -> [CaseAlt] -> Expr
pattern ECase e alts <- ECaseX _ e alts
  where ECase e alts = ECaseX NoExtField e alts

data Arg = UArg Name | TArg Name Type

-- case alternative: constructor name, bound variables, expression
data CaseAltX x = CaseAlt Name [Arg] (ExpX x)
type CaseAlt = CaseAltX FunUD
-- deriving instance Show CaseAlt

data Stmt ann             -- ann - annotation (e.g. stmt before desugar)
    = SExpr ann Expr
--    | SAssign ann Expr Expr
    | SAlloc ann Name Type
    | SInit ann Name Expr

data DeclX x
    = TypeDecl Type [ConAlt]
    | ValDecl Name (Qual Type)
    | ValBind Name [Arg] (ExpX x)
    | Mutual [DeclX x]
    | InstDecl (Qual Pred) [DeclX x]
    | ClsDecl Pred [DeclX x]
    | Pragma String
  -- deriving (Show)
type Decl = DeclX FunUD
instance Show Decl where show = showDecl

data BindX x = Bind
  { bindName :: Name
  , bindArgs :: [Arg]
  , bindBody :: ExpX x
}
type Bind = BindX FunUD
deriving  instance Show Bind

data ConAlt = ConAlt Name [Type]
  -- deriving (Show)

instance Show ConAlt where
    show (ConAlt n ts) = unwords [n, unwords (map show ts)]

data ProgX x = Prog [DeclX x]
type Prog = ProgX FunUD


instance Show Expr where
  showsPrec d (EInt n) = showsPrec 10 n
  showsPrec d (EVar n) = showString n
  showsPrec d (ECon n) = showString n
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec ap_prec e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
         where ap_prec = 10

  showsPrec d (ELam args e) = showParen (d > lam_prec) $
             showString "\\" . showArgs args . showString " -> " .
             showsPrec lam_prec e
         where
           lam_prec = 1
           showArgs = showString . unwords  . map showArg

  showsPrec d (ELet x e1 e2) = showParen (d > let_prec) $
             showString "let " . showString x . showString "= " . showsPrec 0 e1 .
             showsPrec let_prec e2
         where let_prec = 2

  showsPrec d (EBlock stmts) = showString "{\n  " .
                               showString ( intercalate ";\n  " (map show stmts)) .
                               showString "\n}"
  showsPrec d (ETyped e t) = showParen (d > typ_prec) $
             showsPrec 0 e .
             showString " : " . showsPrec 10 t
         where typ_prec = 2
  showsPrec d (ECase e alts) = showString "case " . showsPrec 0 e . showString " of {" .
                               showString ( intercalate ";\n  " (map show alts)) . ('}' :)


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
    show :: Stmt ann -> String
    show (SExpr _ e) = showExpr e
    show (SAlloc _ x t) = concat ["let ",  x, " : ", show t]

instance Show Arg where show :: Arg -> String
                        show = showArg

showArg :: Arg -> String
showArg (UArg s) = s
showArg (TArg s t) = concat ["(",s,":",show t,")"]

instance Show CaseAlt where
    show (CaseAlt c args e) = concat [c, " ", unwords (map show args), " -> ", show e]


showDecl (ValDecl n qt) = unwords [n, ":", show qt]
showDecl (ValBind n [] e) = unwords [n, "=", show e]
showDecl (ValBind n as e) = unwords [n, sas, "=", show e] where
    sas = unwords (map showArg as)
showDecl (ClsDecl pred mdecls) = unwords ["class", show pred, "{",  showDecls mdecls, "}"] where
    showDecls ds = intercalate "; " (map showDecl ds)
showDecl (InstDecl pred mdecls) = unwords ["instance", show pred]
-- showDecl d = show d

argName :: Arg -> Name
argName (UArg s) = s
argName (TArg s t) = s

class ToStr a where
  str :: a -> String

instance {-# OVERLAPPABLE  #-} Show a => ToStr a where str = show
instance {-# OVERLAPPING   #-} ToStr String where str = id
instance {-# OVERLAPPING   #-} ToStr Expr where str = showExpr
-- instance {-# OVERLAPPING   #-} ToStr Decl where str = showDecl

class HasFreeVars a where
    freeVars :: a -> [Name]

instance HasFreeVars Expr where
    freeVars (EVar n) = [n]
    freeVars (EApp e1 e2) = freeVars e1 `union` freeVars e2
    freeVars (ELam args e) = freeVars e \\ map argName args
    freeVars _ = [] -- FIXME
    -- freeVars e = error("freeVars unimplemented for: " ++show e)


instance HasTypes Arg where
    apply s (UArg n) = UArg n
    apply s (TArg n t) = TArg n (apply s t)
    ftv (UArg n) = []
    ftv (TArg n t) = ftv t

instance HasTypes Expr where
    apply s (EInt i) = EInt i
    apply s (EVar n) = EVar n
    apply s (ECon n) = ECon n
    apply s (EApp e1 e2) = EApp (apply s e1) (apply s e2)
    apply s (ELam args e) = ELam (apply s args) (apply s e)
    apply s (ELet n e1 e2) = ELet n (apply s e1) (apply s e2)
    apply s (ETyped e t) = ETyped (apply s e) (apply s t)
    apply s (EBlock stmts) = EBlock (apply s stmts)
    ftv (EInt _) = []
    ftv (EVar n) = []
    ftv (ECon n) = []
    ftv (EApp e1 e2) = ftv e1 ++ ftv e2
    ftv (ELam args e) = ftv args ++ ftv e
    ftv (ELet n e1 e2) = ftv e1 ++ ftv e2
    ftv (ETyped e t) = ftv e ++ ftv t
    ftv (EBlock stmts) = ftv stmts

instance HasTypes (Stmt ann) where
    apply s (SExpr ann e) = SExpr ann (apply s e)
    apply s (SAlloc ann n t) = SAlloc ann n (apply s t)
    ftv (SExpr _ e) = ftv e
    ftv (SAlloc _ n t) = ftv t
