{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Language.Fun.ISyntax
( Name, Expr, TcExpr
, ExpX( .., ELam, ELet, EApp, EVapp, EVar, ECon
       , EInt, EBlock, ETyped, ECase
       , TcInt, TcApp )
, XLam, XLet, XApp, XVapp, XVar, XCon
, XInt, XBlock, XTyped, XCase, XExp
, Arg(..), argName
, CaseAltX(.., CaseAlt), XCaseAlt, CaseAlt, TcCaseAlt
, Stmt, StmtX(.., SExpr, SAlloc, SInit), TcStmt
, XSExpr, XSAlloc, XSInit, XStmt
, DeclX(..), Decl, TcDecl
, BindX(..), Bind, TcBind
, ConAlt(..)
, ProgX(..), Prog, TcProg
, ToStr(..), HasTypes(..), HasFreeVars(..)
, showExpr, showDecl
, typeOfTcExpr, typeOfTcStmt
) where
import Data.List(union, intersect, nub, (\\), intercalate)
import Language.Fun.Types
import Language.Fun.Constraints(HasTypes(..))
import Language.Fun.Phase
    ( absurd, DataConCantHappen, FunTc, FunUD, NoExtField(..), FunDs )

type Name = String

data ExpX x
    = ELamX (XLam x) [Arg] (ExpX x)         -- function \args -> expr
    | ELetX (XLet x) Name (ExpX x) (ExpX x) -- local definition: let name = expr1 in expr2
    | EAppX (XApp x) (ExpX x) (ExpX x)         -- function call: f(arg)
    | EVappX (XApp x) (ExpX x) [(ExpX x)]      -- vector application: f(args)
    | EVarX  (XVar x) Name              -- variable
    | EConX (XCon x) Name              -- value constructor
    | EIntX (XInt x) Integer           -- integer literal
    | EBlockX (XBlock x) [StmtX x]   -- desugared statements annotated with their source form
    | ETypedX (XTyped x) (ExpX x) Type
    | ECaseX (XCase x) (ExpX x) [CaseAltX x]
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

type Expr = ExpX FunDs

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

type instance XLam FunDs = NoExtField
type instance XLet FunDs = NoExtField
type instance XApp FunDs = NoExtField
type instance XVapp FunDs = NoExtField
type instance XVar FunDs = NoExtField
type instance XCon FunDs = NoExtField
type instance XInt FunDs = NoExtField
type instance XBlock FunDs = NoExtField
type instance XTyped FunDs = NoExtField
type instance XCase FunDs = NoExtField
type instance XExp FunDs = DataConCantHappen

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

pattern EBlock :: [Stmt] -> Expr
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
type family XCaseAlt x

data CaseAltX x = CaseAltX (XCaseAlt x) Name [Arg] (ExpX x)
type CaseAlt = CaseAltX FunDs

type instance XCaseAlt FunUD = NoExtField
type instance XCaseAlt FunDs = NoExtField

pattern CaseAlt :: Name -> [Arg] -> Expr -> CaseAlt
pattern CaseAlt c args e <- CaseAltX _ c args e
  where CaseAlt c args e = CaseAltX NoExtField c args e

-- deriving instance Show CaseAlt

data StmtX x             -- ann - annotation (e.g. stmt before desugar)
    = SExprX (XSExpr x) (ExpX x)
--    | SAssign ann Expr Expr
    | SAllocX (XSAlloc x) Name Type
    | SInitX (XSInit x) Name (ExpX x)
    | StmtX (XStmt x)

type family XSExpr x
type family XSAlloc x
type family XSInit x
type family XStmt x

type instance XSExpr FunUD = NoExtField
type instance XSAlloc FunUD = NoExtField
type instance XSInit FunUD = NoExtField
type instance XStmt FunUD = DataConCantHappen

type instance XSExpr FunDs = String
type instance XSAlloc FunDs = String
type instance XSInit FunDs = String
type instance XStmt FunDs = DataConCantHappen

type Stmt = StmtX FunDs
pattern SExpr :: String -> ExpX FunDs -> Stmt
pattern SExpr s e = SExprX s e
pattern SAlloc :: XSAlloc x -> Name -> Type -> StmtX x
pattern SAlloc s n t = SAllocX s n t
pattern SInit s n e = SInitX s n e

data DeclX x
    = TypeDecl Type [ConAlt]
    | ValDecl Name (Qual Type)
    | ValBind Name [Arg] (ExpX x)
    | Mutual [DeclX x]
    | InstDecl (Qual Pred) [DeclX x]
    | ClsDecl Pred [DeclX x]
    | Pragma String
  -- deriving (Show)
type Decl = DeclX FunDs
instance Show Decl where show = showDecl

data BindX x = Bind
  { bindName :: Name
  , bindArgs :: [Arg]
  , bindBody :: ExpX x
}
type Bind = BindX FunDs
deriving  instance Show Bind

data ConAlt = ConAlt Name [Type]
  -- deriving (Show)

instance Show ConAlt where
    show (ConAlt n ts) = unwords [n, unwords (map show ts)]

data ProgX x = Prog [DeclX x]
type Prog = ProgX FunDs


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

instance Show Stmt where
    show :: Stmt -> String
    show (SExpr _ e) = showExpr e
    show (SAlloc _ x t) = concat ["let ",  x, " : ", show t]

instance Show Arg where show :: Arg -> String
                        show = showArg

showArg :: Arg -> String
showArg (UArg s) = s
showArg (TArg s t) = concat ["(",s,":",show t,")"]

instance Show CaseAlt where
    show (CaseAltX _ c args e) = concat [c, " ", unwords (map show args), " -> ", show e]


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

instance HasTypes Stmt where
    apply s (SExpr ann e) = SExpr ann (apply s e)
    apply s (SAlloc ann n t) = SAlloc ann n (apply s t)
    ftv (SExpr _ e) = ftv e
    ftv (SAlloc _ n t) = ftv t

-- Typechecked

type instance XLam FunTc = NoExtField
type instance XLet FunTc = NoExtField
type instance XApp FunTc = NoExtField
type instance XVapp FunTc = NoExtField
type instance XVar FunTc = Type
type instance XCon FunTc = Type
type instance XInt FunTc = NoExtField
type instance XBlock FunTc = NoExtField
type instance XTyped FunTc = NoExtField
type instance XCase FunTc = Type
type instance XExp FunTc = DataConCantHappen

type TcExpr = ExpX FunTc
pattern TcInt :: Integer -> TcExpr
pattern TcInt n = EIntX NoExtField n
pattern TcApp :: TcExpr -> TcExpr -> TcExpr
pattern TcApp e1 e2 = EAppX NoExtField e1 e2

type TcDecl = DeclX FunTc
type TcProg = ProgX FunTc
type TcBind = BindX FunTc

type TcStmt = StmtX FunTc
type instance XSExpr FunTc = NoExtField
type instance XSAlloc FunTc = NoExtField
type instance XSInit FunTc = NoExtField
type instance XStmt FunTc = DataConCantHappen

type TcCaseAlt = CaseAltX FunTc
type instance XCaseAlt FunTc = NoExtField

instance HasTypes TcExpr where
  apply s (ELamX _ args e) = ELamX NoExtField args (apply s e)
  apply s (ELetX _ x e1 e2) = ELetX NoExtField x (apply s e1) (apply s e2)
  apply s (EAppX _ e1 e2) = EAppX NoExtField (apply s e1) (apply s e2)
  apply s (EVappX _ e es) = EVappX NoExtField (apply s e) (map (apply s) es)
  apply s (EVarX t n) = EVarX (apply s t) n
  apply s (EConX t n) = EConX (apply s t) n
  apply s (EIntX _ n) = EIntX NoExtField n
  apply s (EBlockX _ stmts) = EBlockX NoExtField stmts
  apply s (ETypedX _ e t) = ETypedX NoExtField (apply s e) t
  apply s (ECaseX t e alts) = ECaseX (apply s t) (apply s e) alts
  apply s (ExpX a) = absurd a
  ftv (ELamX _ args e) = ftv e
  ftv (ELetX _ _ e1 e2) = ftv e1 `union` ftv e2
  ftv (EAppX _ e1 e2) = ftv e1 `union` ftv e2
  ftv (EVappX _ e es) = foldr union [] (map ftv (e:es))
  ftv (EVarX t _) = ftv t
  ftv (EConX t _) = ftv t
  ftv (EIntX _ _) = []
  ftv (EBlockX _ stmts) = []
  ftv (ETypedX _ e _) = ftv e
  ftv (ECaseX t e alts) = ftv t `union` ftv e `union` foldr union [] (map ftv alts)

instance HasTypes TcCaseAlt where
  apply s (CaseAltX x n args e) = CaseAltX x n args (apply s e)
  ftv (CaseAltX x _ args e) = ftv args `union` ftv e

instance Show TcExpr where
  showsPrec d (EIntX _ n) = showsPrec 10 n
  showsPrec d (EVarX t n) = showParen True $
             showString n .showString ": " . shows t
  showsPrec d (EConX t n) = showParen True $
             showString n .showString ": " . shows t
  showsPrec d (EAppX _ e1 e2) = showParen (d > ap_prec) $
             showsPrec ap_prec e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
         where ap_prec = 10

  showsPrec d (ELamX _ args e) = showParen (d > lam_prec) $
             showString "\\" . showArgs args . showString " -> " .
             showsPrec lam_prec e
         where
           lam_prec = 1
           showArgs = showString . unwords  . map showArg

  showsPrec d (ELetX _ x e1 e2) = showParen (d > let_prec) $
             showString "let " . showString x . showString "= " . showsPrec 0 e1 .
             showsPrec let_prec e2
         where let_prec = 2

  showsPrec d (EBlockX _ stmts) = showString "{\n  " .
                               showString ( intercalate ";\n  " (map show stmts)) .
                               showString "\n}"
  showsPrec d (ETypedX _ e t) = showParen (d > typ_prec) $
             shows e .
             showString " : " . shows t
         where typ_prec = 2
  showsPrec d (ECaseX t e alts) = showString "case<" . shows t . showString "> " .
                               shows e . showString " of {\n  " .
                               showString ( intercalate ";\n  " (map show alts)) .
                               showString "\n}"

instance Show TcCaseAlt where
  show (CaseAltX _ n args e) = unwords [n, showArgs args, "->", show e] where
    showArgs = unwords . map showArg

instance Show TcStmt where
    show (SExprX _ e) = show e
    show (SAllocX _ x t) = concat ["let ",  x, " : ", show t]

{-
showArg :: Arg -> String
showArg (UArg s) = s
showArg (TArg s t) = concat ["(",s,":",show t,")"]
-}

instance Show TcDecl where
  show :: TcDecl -> String
  show (TypeDecl t alts) = unwords [show t, "=", salts]
     where salts = intercalate " | " (map show alts)
  show (ValDecl n qt) = unwords [n, ":", show qt]
  show (ValBind n [] e) = unwords [n, "=", show e]
  show (ValBind n as e) = unwords [n, sas, "=", show e] where
    sas = unwords (map showArg as)
  show (ClsDecl pred mdecls) = unwords ["class", show pred, "{",  showDecls mdecls, "}"] where
    showDecls ds = intercalate "; " (map showDecl ds)
  show (InstDecl pred mdecls) = unwords ["instance", show pred]
  show (Mutual ds) = "mutual {" ++ intercalate ";\n " (map show ds) ++ "}"
  show (Pragma prag) = "pragma " ++ prag

-- The typechecked expression should be annotated enough
-- so that we can easily extract its type
typeOfTcExpr :: TcExpr -> Type
typeOfTcExpr e = go e where
  argType (TArg _ t) = t
  argType (UArg _) = error "typeOfTcExpr: untyped argument" --FIXME: make this case really impossible
  go (EIntX _ _) = TInt
  go (EVarX t _) = t
  go (EConX t _) = t
  go (EAppX _ e1 e2) = case go e1 of
    (_ :-> t2) -> t2
    _ -> error("typeOfTcExpr: " ++ show e1 ++ " is not a function")
  go (EVappX _ e es) = case go e of
    (_ :-> t2) -> t2
    _ -> error("typeOfTcExpr: " ++ show e ++ " is not a function")
  go (ELamX _ args e) = foldr (:->) (go e) (map argType args)
  go (ELetX _ _ e1 e2) = go e2
  go (ETypedX _ _ t) = t
  go (EBlockX _ stmts) = typeOfTcStmts stmts
  go (ECaseX t _ _) = t
  go (ExpX x) = absurd x

typeOfTcStmts :: [TcStmt] -> Type
typeOfTcStmts [stmt] = typeOfTcStmt stmt
typeOfTcStmts (stmt:stmts) = typeOfTcStmts stmts
typeOfTcStmts [] = TUnit

typeOfTcStmt :: TcStmt -> Type
typeOfTcStmt (SExprX _ e) = typeOfTcExpr e
typeOfTcStmt (SAllocX _ _ t) = TUnit
typeOfTcStmt (SInitX _ _ t) = TUnit