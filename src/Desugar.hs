{-# LANGUAGE FunctionalDependencies #-}
module Desugar where
import Syntax
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
