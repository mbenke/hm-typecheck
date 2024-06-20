{-# LANGUAGE LambdaCase #-}
module Check where
import Exp
  ( Exp, ExpX(..), ExpTc(..), Typ(..)
  )

import TCM
import Unify ( HasTypes(apply) )
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad.Reader.Class ( MonadReader(local) )




infer :: Exp -> TCM (ExpTc, Typ)
infer (Lit n) = pure (LitX mempty n, TInt)

infer (Var v) = do
    typ <- askType v
    pure $ (VarX typ v, typ)

infer (Ann e t) = do
    (e', t2) <- infer e 
    unify t2 t
    pure (AnnX mempty e' t, t)

infer (Abs v e) = do
    tv <- freshName
    let t = TVar tv
    (e',t') <- local (extEnv v t) $ infer e
    pure (AbsX t v e', t :-> t')

infer (App e1 e2) = do
    (e1', t1) <- infer e1
    (e2', t2) <- infer e2
    tv <- freshName
    let t = TVar tv
    unify t1 (t2 :-> t)
    pure (AppX mempty e1' e2', t)

infer _ = throwError "infer: not implemented"

doInfer :: Exp -> TCM (ExpTc, Typ)
doInfer e = infer e >>= applyCurrentSubst
