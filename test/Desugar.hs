{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Desugar where
import Syntax
import qualified Syntax as C
import qualified ISyntax as I
import Types

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

instance Desugar C.Arg Name where
  desugar (C.UArg x) = desugar x

instance Desugar C.Expr I.Expr where
  desugar (C.ELam args e) = I.ELam (map desugar args) (desugar e)
  desugar (C.ELet i e1 e0) = I.ELet (desugar i) (desugar e1) (desugar e0)
  desugar (C.EApp e1 e2) = I.EApp (desugar e1) (desugar e2)
  desugar (C.EVar i) = I.EVar (desugar i)
  desugar (C.ECon i) = I.ECon (desugar i)
  desugar (C.EInt n) = I.EInt n

instance Desugar C.Decl I.Decl where
  desugar (C.TypeDecl ct rhs) = I.TypeDecl (desugar ct) (desugar rhs)
  desugar (C.ValDecl i qt) = I.ValDecl (desugar i) (desugar qt)
  desugar (C.ValBind i args e) = I.ValBind (desugar i) (map desugar args) (desugar e)
  desugar (C.InstDecl qp) = I.InstDecl (desugar qp)
  desugar (C.ClsDecl p ms) = I.ClsDecl (desugar p) (desugar ms)

instance Desugar C.Methods [I.Decl] where
  desugar C.NoMethods = []
  desugar (C.SomeMethods ds) = map desugar ds

instance Desugar C.TyDeRhs [I.ConAlt] where
  desugar (C.EmptyTyDeRhs) = []
  desugar (C.ConAlts cas) = map desugar cas

instance Desugar C.ConAlt I.ConAlt where
  desugar (C.ConAlt0 ui) = I.ConAlt (desugar ui) []
  desugar (C.ConAltN ui cts) = I.ConAlt (desugar ui) (map desugar cts)

instance Desugar C.Prog I.Prog where
  desugar (C.Prog ds) = I.Prog (map desugar ds)

desugarT :: CType -> Type
desugarT (CTVar i) = TVar (name i)
desugarT (CTArr t u) = desugarT t :-> desugarT u
desugarT (CTCon i ts) = TCon (name i) (map desugarT ts)
desugarT (CTCon0 i) = TCon (name i) []

desugarP :: CPred -> Pred
desugarP (PSingle ct i) = InCls (name i) [] (desugarT ct)
desugarP (PMulti  ct i cts) = InCls (name i) (map desugarT cts) (desugarT ct)

-- desugar a qualified Pred or Type
desugarQ :: Desugar c a => [CPred] -> c -> Qual a
desugarQ cps c = map desugarP cps :=> desugar c
