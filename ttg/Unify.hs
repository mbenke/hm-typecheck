module Unify where
import Exp
import Control.Monad.Error.Class
import Data.List(union, intersect, nub, (\\), deleteBy, intercalate)
import Text.Show(showListWith)

newtype Subst = Subst [(Tyvar,Typ)]

instance Show Subst where
  showsPrec _ (Subst ps) = showListWith showsPair ps where
    showsPair :: (Tyvar,Typ) -> ShowS
    showsPair (v, t) = showString v .  showString ":=" . shows t

emptySubst :: Subst
emptySubst = Subst []

(+->)      :: Tyvar -> Typ -> Subst
u +-> t     = Subst [(u, t)]

expel :: [Tyvar] -> Subst -> Subst
expel [] s = s
expel (v:vs) (Subst s) = Subst $ filter ((v/=).fst) s

class HasTypes a where
    apply :: Subst -> a -> a
    ftv   :: a -> [Tyvar]

instance HasTypes Typ where
    apply (Subst s) t@(TVar u) = case lookup u s of
      Nothing -> t
      Just v -> v
    apply s (l:->r) = apply s l :-> apply s r
    apply _ t = t

    -- ftv :: Typ -> [Tyvar]
    ftv (TVar u) = [u]
    ftv (l:->r)  = ftv l `union` ftv r
    ftv _ = []

instance HasTypes a => HasTypes [a] where
    apply s = map (apply s)
    ftv = nub . concatMap ftv

instance (HasTypes a, HasTypes b) => HasTypes (a,b) where
    apply s (x,y) = (apply s x, apply s y)  
    ftv (x,y) = ftv x `union` ftv y 

instance Semigroup Subst where
  -- apply (s1 <> s2) = apply s1 . apply s2
  s1@(Subst ps1) <> s2@(Subst ps2) =  Subst $ [ (u, apply s1 t) | (u,t) <- ps2 ] ++ ps1

instance Monoid Subst where
  mempty = emptySubst

mgu     :: MonadError String m => Typ -> Typ -> m Subst
varBind :: MonadError String m => Tyvar -> Typ -> m Subst


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
mgu (l:->r) (l':->r') = do s1 <- mgu l l'
                           s2 <- mgu (apply s1 r) (apply s1 r')
                           return (s2 <> s1)
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TInt) (TInt)     = return mempty
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

errInfinite :: Typ -> Typ -> String
errInfinite u t = unwords [
  "Cannot construct the infinite type:",
  show u,
  "~",
  show t
  ]


type E a = Either String a
testmgu :: Typ -> Typ -> E Subst
testmgu = mgu


instance HasTypes ExpTc where
    apply s (LitX _ n) = LitX mempty n
    apply s (VarX t v) = VarX (apply s t) v
    apply s (AnnX _ e t) = AnnX mempty (apply s e) (apply s t)
    apply s (AbsX t v e) = AbsX (apply s t) v (apply s e)
    apply s (AppX _ e1 e2) = AppX mempty (apply s e1) (apply s e2)
    apply s (ExpX x) = absurd x

    ftv (LitX _ _) = []
    ftv (VarX _ _) = []
    ftv (AnnX _ e t) = ftv e `union` ftv t
    ftv (AbsX _ _ e) = ftv e
    ftv (AppX _ e1 e2) = ftv e1 `union` ftv e2
    ftv (ExpX x) = absurd x