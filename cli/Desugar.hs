{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Desugar where
import qualified AbsFun as C
import AbsFun(UIdent(..), LIdent(..), CType(..), CPred(..), QType(..), QPred(..))
import PrintFun(printTree)
import qualified ISyntax as I
import Types
import Common.Debug

type Name = String

class Desugar c a | c -> a where
  desugar :: c -> a

instance Desugar CType Type where
  desugar = desugarT

instance Desugar CPred Pred where
  desugar = desugarP

instance Desugar QPred (Qual Pred) where
  desugar (I0Qual p) = desugarQ [] p
  desugar (I1Qual q p) = desugarQ [q] p
  desugar (INQual qs p) = desugarQ qs p

instance Desugar QType (Qual Type) where
  desugar (T0Qual p) = desugarQ [] p
  desugar (T1Qual q p) = desugarQ [q] p
  desugar (TNQual qs p) = desugarQ qs p

instance Desugar C.LIdent Name where
  desugar (LIdent s) = s

instance Desugar C.UIdent Name where
  desugar (UIdent s) = s

instance Desugar C.Arg I.Arg where
  desugar (C.UArg x) = I.UArg (desugar x)
  desugar (C.TArg x t) = I.TArg (desugar x) (desugar t)

instance Desugar C.Expr I.Expr where
  desugar (C.ELam args e)  = I.ELam (map desugar args) (desugar e)
  desugar (C.ELet i e1 e0) = I.ELet (desugar i) (desugar e1) (desugar e0)
  desugar (C.EApp e1 e2)   = I.EApp (desugar e1) (desugar e2)
  desugar (C.EMet e1 e2)   = I.EApp (desugar e2) (desugar e1)
  desugar (C.EVar i)       = I.EVar (desugar i)
  desugar (C.ECon i)       = I.ECon (desugar i)
  desugar (C.EInt n)       = I.EInt n
  desugar (C.EBlock stmts) = I.EBlock (map desugar stmts)
  desugar (C.ETyped e t)   = I.ETyped (desugar e) (desugar t)
  desugar (C.ECase e alts) = I.ECase (desugar e) (map desugar alts)
  desugar e = error $ "C.Expr.desugar unimplemented for  " ++ show e

instance Desugar C.CaseAlt I.CaseAlt where
  desugar (C.CaseAlt i args e) = I.CaseAlt (desugar i) (map desugar args) (desugar e)

instance Desugar C.Stmt (I.Stmt String) where
  desugar stmt@(C.SExpr e)     = I.SExpr (printTree stmt) (desugarRhs e)
  desugar stmt@(C.SAlloc i t)  = I.SAlloc (printTree stmt) (desugar i) (desugar t)
  desugar stmt@(C.SInit i e)   = I.SInit (printTree stmt) (desugar i) (desugar e)
--  desugar (C.SAssign i e) = I.SAssign (desugar i) (desugar e)
  desugar stmt@(C.SAssign e1 e2) = I.SExpr (printTree stmt) (store lhs rhs) where
      store x y = I.EApp (I.EApp (I.EVar "store") lhs) rhs
      lhs = desugarLhs e1
      rhs = desugarRhs e2

desugarLhs :: C.Expr -> I.Expr
desugarLhs (C.EStar i) = I.EApp (I.EVar "load") (I.EVar (desugar i))
desugarLhs (C.EApp f a) = I.EApp (desugarLhs f) (desugarRhs a)
desugarLhs (C.EMet a f) = I.EApp (desugarLhs f) (desugarLhs a)
desugarLhs (C.EIdx a i) = access (desugarRhs a) (desugarRhs i) where
    access a i = I.EApp (I.EApp (I.EVar "indexAccess") a) i
desugarLhs e = desugar e

desugarRhs :: C.Expr -> I.Expr
desugarRhs e = go e where
    go e@(C.EVar i) = load (I.EVar (desugar i))
    go e@(C.EStar i) = load (desugarLhs e)
    go e@(C.EApp f a) = I.EApp (desugarLhs f) (desugarRhs a)
    go e@(C.EMet a f) = load $ I.EApp (desugar f) (desugar a)
    go e@(C.EIdx a i) = load (desugarLhs e)
    go e = desugar e
    load = I.EApp (I.EVar "load")


instance Desugar C.Decl I.Decl where
  desugar (C.TypeDecl ct rhs)  = I.TypeDecl (desugar ct) (desugar rhs)
  desugar (C.ValDecl i qt)     = I.ValDecl (desugar i) (desugar qt)
  desugar (C.ValBind i args e) = I.ValBind (desugar i) (map desugar args) (desugar e)
  desugar (C.Mutual ds)        = I.Mutual (map desugar ds)
  desugar (C.InstDecl qp ms)   = I.InstDecl (desugar qp) (desugar ms)
  desugar (C.ClsDecl p ms)     = I.ClsDecl (desugar p) (desugar ms)
  desugar (C.Pragma i)         = I.Pragma (desugar i)

instance Desugar C.Methods [I.Decl] where
  desugar C.NoMethods        = []
  desugar (C.SomeMethods ds) = map desugar ds

instance Desugar C.TyDeRhs [I.ConAlt] where
  desugar (C.EmptyTyDeRhs) = []
  desugar (C.ConAlts cas)  = map desugar cas

instance Desugar C.ConAlt I.ConAlt where
  desugar (C.ConAlt0 ui)     = I.ConAlt (desugar ui) []
  desugar (C.ConAltN ui cts) = I.ConAlt (desugar ui) (map desugar cts)

instance Desugar C.Prog I.Prog where
  desugar (C.Prog ds) = I.Prog (map desugar ds)

desugarT :: CType -> Type
desugarT (CTVar i)    = TVar (desugar i)
desugarT (CTArr t u)  = desugarT t :-> desugarT u
desugarT (CTCon i ts) = TCon (desugar i) (map desugarT ts)
desugarT (CTCon0 i)   = TCon (desugar i) []

desugarP :: CPred -> Pred
desugarP (PSingle ct i)     = InCls (desugar i) [] (desugarT ct)
desugarP (PMulti  ct i cts) = InCls (desugar i) (map desugarT cts) (desugarT ct)

-- desugar a qualified Pred or Type
desugarQ :: Desugar c a => [CPred] -> c -> Qual a
desugarQ cps c = map desugarP cps :=> desugar c
