{-# LANGUAGE FlexibleContexts #-}
module MLConstraints where
import Control.Monad.Error.Class
import Data.List(union, intersect, nub, (\\), deleteBy, intercalate)
import Text.Show(showListWith)
import MLTypes

-- Using ideas from "Typing Haskell in Haskell" by Mark P. Jones

type Constraint = (Type,Type)
type Constraints = [Constraint]

newtype Subst = Subst [(Tyvar,Type)]

instance Show Subst where
  -- show (Subst ps) = intercalate "," (map showPair ps) where
  showsPrec _ (Subst ps) = showListWith showsPair ps where
    showsPair :: (Tyvar,Type) -> ShowS
    showsPair (v, t) = showString v .  showString ":=" . shows t

emptySubst :: Subst
emptySubst = Subst []

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = Subst [(u, t)]

expel :: [Tyvar] -> Subst -> Subst
expel [] s = s
expel (v:vs) (Subst s) = Subst $ filter ((v/=).fst) s

class HasTypes a where
    apply :: Subst -> a -> a
    ftv   :: a -> [Tyvar]

instance HasTypes Type where
    apply (Subst s) t@(TVar u) = case lookup u s of
      Nothing -> t
      Just v -> v
    apply s (TCon n ts) = TCon n (map (apply s) ts)

    -- ftv :: Type -> [Tyvar]
    ftv (TVar u) = [u]
    ftv (TCon n ts) = ftv ts -- foldr union [] (map ftv ts)

instance HasTypes a => HasTypes [a] where
  apply s = map (apply s)
  ftv = foldr union [] . map ftv

instance (HasTypes a, HasTypes b) => HasTypes (a,b) where
  apply s (a,b) = (apply s a, apply s b)
  ftv (a,b) = ftv a `union` ftv b

instance HasTypes t => HasTypes(Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `union` ftv t

instance HasTypes Pred where
  apply s (InCls i as t) = InCls i (apply s as) (apply s t)
  apply s (t :~: u) = apply s t :~: apply s u
  ftv (InCls i as t) = ftv (as, t)
  ftv (t :~: u) = ftv (t,u)

instance HasTypes Scheme where
  apply subst (Forall tvs t) = Forall tvs (apply (expel tvs subst) t)
  ftv (Forall tvs t) = ftv t \\ tvs

-- this isn't really used, just for experiments
instance HasTypes Subst where
  apply s1 (Subst ps2) = Subst [ (u, apply s1 t) | (u,t) <- ps2 ]
  ftv s = error "ftv not implemented for Subst"

-- infixr 5 @@
-- (@@) :: Subst -> Subst -> Subst

instance Semigroup Subst where
  -- apply (s1 <> s2) = apply s1 . apply s2
  s1@(Subst ps1) <> s2@(Subst ps2) =  Subst $ nub [ (u, apply s1 t) | (u,t) <- ps2 ] ++ ps1

instance Monoid Subst where
  mempty = emptySubst

merge :: MonadError String m => Subst -> Subst -> m Subst
merge s1@(Subst p1) s2@(Subst p2) = if agree then pure (Subst (p1 ++ p2))
                                             else throwError "merge fails"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                (dom p1 `intersect` dom p2)
    dom s = map fst s

match   :: MonadError String m => Type -> Type -> m Subst
mgu     :: MonadError String m => Type -> Type -> m Subst
varBind :: MonadError String m => Tyvar -> Type -> m Subst

match (TCon tc1 as1) (TCon tc2 as2)
  | tc1 == tc2 = matcha as1 as2
  where
    matcha []  [] = pure mempty
    matcha (a:as) (b:bs) = do
      sl <- match a b
      sr <- matcha as bs
      merge sl sr
match (TVar u) t = pure (u +-> t)
match _ _ = throwError "types do not match"

matchTypes :: MonadError String m => [Type] -> [Type] -> m Subst
matchTypes []  [] = pure mempty
matchTypes (a:as) (b:bs) = do
  sl <- match a b
  sr <- matchTypes as bs
  merge sl sr



{-
   Solving (l:->r) ~ (l':->r') amounts to solving [ l ~ l', r ~ r']:
   s1 <- l ~ l'
   apply s1 to rest of the equations (i.e. r ~ r')
   s2 <- (s1 r ~ s1 r')
   return (s2 <> s1)

   IOW

   solve [] = mempty
   solve ((l:->r, l':->r'):es) = solve ((l,l'):(r,r'):es)
   solve ((TInt,TInt):es) = solve es
   solve ((TVar u, t):es) = do { s1 <- varBind u t; s2 <-solve (apply s1 es); return s2<>s1 }
-}
-- solve a list of constraints

solve :: MonadError String m => Constraints -> Subst -> m Subst
solve [] subst = pure subst
solve ((l,r):cs) s = do
  s' <- mgu (apply s l) (apply s r)
  s'' <- solve cs s'
  pure (s'<>s'')


{-
solve ::  MonadError String m => [(Type, Type)] -> m Subst
solve [] = pure mempty
solve ((t1, t2):cs) = do
  st <- solve cs
  sh <- mgu (apply st t1) (apply st t2)
  pure (sh <> st)
-}

{-
mgu (l:->r) (l':->r') = do s1 <- mgu l l'
                           s2 <- mgu (apply s1 r) (apply s1 r')
                           return (s2 <> s1)
-}
mgu (TCon n ts1) (TCon n' ts2) | n == n' && length n == length n'
  = solve (zip ts1 ts2) mempty
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
-- mgu (TInt) (TInt)     = pure mempty
mgu t1 t2             = throwError (unwords
                                    ["types",
                                     quote t1,
                                     "and",
                                     quote t2,
                                     "do not unify"])


quote :: Show a => a -> String
quote t = "'" ++ show t ++ "'"


varBind u t | t == TVar u      = return emptySubst
            | u `elem` ftv t   = throwError (errInfinite (TVar u) t)
            | otherwise        = return (u +-> t)

errInfinite :: Type -> Type -> String
errInfinite u t = unwords [
  "Cannot construct the infinite type:",
  show u,
  "~",
  show t
  ]

unifyTypes :: MonadError String m => [Type] -> [Type] -> m Subst
unifyTypes ts us = solve (zip ts us) mempty


onPred m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = throwError "classes differ"

mguPred, matchPred :: Pred -> Pred -> Either String Subst
mguPred (InCls i as t) (InCls i' as' t')
  | i == i' = unifyTypes (t:as) (t':as')
  | otherwise = throwError "classes differ"

matchPred (InCls i as t) (InCls i' as' t')
  | i == i' = matchTypes (t:as) (t':as')
  | otherwise = throwError "classes differ"
