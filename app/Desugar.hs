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

desugarT :: CType -> Type
desugarT (CTVar i) = TVar (name i)
desugarT (CTArr t u) = desugarT t :-> desugarT u
desugarT (CTCon i ts) = TCon (name i) (map desugarT ts) 
desugarT (CTCon0 i) = TCon (name i) []

desugarP :: CPred -> Pred
desugarP (PSingle ct i) = InCls (name i) [] (desugarT ct)
desugarP (PMulti  ct i cts) = InCls (name i) (map desugarT cts) (desugarT ct)

desugarQ :: [CPred] -> CPred -> Qual Pred
desugarQ cps cp = map desugarP cps :=> desugarP cp
