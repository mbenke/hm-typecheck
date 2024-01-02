{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module MLChecker where
-- import Prelude hiding(lookup)
import Data.List((\\))
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import MLExpr
-- import MLSyntax
import MLTypes
import NameSupply
import MLConstraints

type Env = Map.Map Name Scheme

emptyEnv :: Env
emptyEnv = Map.empty

prims =
  [ ("zero", monotype $ int :-> int :-> int)
  , ("add", monotype $ int :-> int :-> int)
  , ("ifzero", Forall ["a"] $ int :-> a :-> a :-> a )
  ] where a = TVar "a"

primEnv :: Env
primEnv = Map.fromList prims
{-
instance HasTypes Env where
  ftv env = concatMap fetch (Map.toList env) where
    fetch (_,s) = ftv s
  apply = undefined
-}

data TcState = TcState {
 tcsNS :: NS,
 tcsSubst :: Subst,
 constraints :: Constraints
}

type TCM a = ExceptT String (StateT TcState (Reader Env)) a

askType :: Name -> TCM Scheme
askType n = do
  result <- asks (Map.lookup n)
  case result of
    Just t -> return t
    Nothing -> throwError $ "Unknown name: " ++ n

askFreeVars :: TCM [Tyvar]
askFreeVars = asks (concatMap (ftv.snd) . Map.toList)

tcsDeplete :: TcState -> (String,TcState)
tcsDeplete s = (name, s { tcsNS = ns' }) where
   ns = tcsNS s
   (name,ns') = deplete ns

nssDeplete :: Monad m => StateT TcState m String
nssDeplete = StateT $ return . tcsDeplete

tcmDeplete :: TCM String
tcmDeplete = lift nssDeplete

getSubst :: TCM Subst
getSubst = gets tcsSubst

unify :: Type -> Type -> TCM ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 modify(extSubst u)

extSubst   :: Subst -> TcState -> TcState
extSubst s st = st { tcsSubst = s <> s0 } where s0 = tcsSubst st

addConstraint :: Constraint -> TCM ()
addConstraint c = modify (addC c) where
    addC c s = s { constraints = c:constraints s }

freshInst :: Scheme -> TCM Type
freshInst (Forall tvs ty) = do
  s <- Subst <$> mapM newTVar tvs
  return (apply s ty)

newTVar :: Tyvar -> TCM (Tyvar, Type)
newTVar tv = do
  n <- tcmDeplete
  return (tv, TVar n)

addMonoBind :: Name -> Type -> Env -> Env
addMonoBind n t = Map.insert n s where
  s = Forall [] t

addPolyBind :: Name -> Scheme -> Env -> Env
addPolyBind n s = Map.insert n s

typeOf0 :: Expr -> TCM Type
typeOf0 (EInt _) = return TInt
typeOf0 (ELam ident e1) = do
  let n = name ident
  freshName <- tcmDeplete
  let t0 = TVar freshName
  t1 <- local (addMonoBind n t0) $ typeOf0 e1
  return $ t0 :-> t1

typeOf0 (EVar n) = do
  s <- askType (name n)
  freshInst s

typeOf0 (EApp e1 e2) = do
  t1 <- typeOf0 e1
  t2 <- typeOf0 e2
  freshname <- tcmDeplete
  let tr = TVar freshname
  unify t1 (t2 :-> tr)
  s <- getSubst
  return $ apply s tr

typeOf0 exp@(ELet ident e1 e2) = do
  let n = name ident
  t1 <- typeOf0 e1
  s <- generalize t1
  local (addPolyBind n s) (typeOf0 e2)

typeOf0 (ERec [] e) = typeOf0 e
typeOf0 _ = error "ERec not implemented" -- TODO

generalize :: Type -> TCM Scheme
generalize t = do
  envVars <- askFreeVars
  let typeVars =  ftv t
  return $ Forall (typeVars \\ envVars) t


isFreeInEnv :: Tyvar -> TCM Bool
isFreeInEnv tv = return False -- FIXME

runEnvR r =  runReader r primEnv

runTcS t = runStateT t initState
initState = TcState namePool emptySubst []

runTCM :: TCM a -> (Either String a, TcState)
runTCM = runEnvR . runTcS . runExceptT

evalTCM :: TCM a -> Either String a
evalTCM = fst. runTCM

typeOf1 :: Expr -> TCM Scheme
typeOf1 exp = catchError ty handler where
  ty = (typeOf0 exp) >>= (return . Forall []) -- generalize
  handler :: String -> TCM Scheme
  handler e = throwError $
    "Type error in\n  "
    ++show exp++"\n"
    ++e

typeOf :: Expr -> Either String Scheme
typeOf exp = apply subst <$> t where
  (t,state) = runTCM (typeOf1 exp)
  subst = tcsSubst state

typeCheck :: Expr -> IO ()
typeCheck exp = case typeOf exp of
                  Left e -> putStrLn "Error: " >> putStrLn e
                  Right t -> putStrLn $ (showExpr exp) ++ " :: " ++ show t

testCheck :: Expr -> IO ()
testCheck exp = case result of
                  Left e -> do
                    putStrLn "Error: "
                    putStrLn e
                    putStrLn "Constraints:"
                    print cons
                  Right t -> do
                    let typ = apply subst t
                    print typ
                    putStrLn "Constraints:"
                    print cons
                    putStrLn "Substitution:"
                    print subst
  where
    (result,state) = runTCM (typeOf1 exp)
    cons = constraints state
    subst = tcsSubst state

solve :: Constraints -> Subst -> TCM Subst
solve [] subst = return subst
solve ((l,r):cs) s = do
  s' <- mgu l r
  solve cs s'

unifError :: Constraint -> TCM a
unifError (t1,t2) = throwError $ "Cannot unify "++show t1++" with "++show t2
