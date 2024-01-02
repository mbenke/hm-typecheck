{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
module TCM where
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map

import NameSupply
import MLConstraints
import MLExpr

import Prims(primVals, primTypes, primClasses)
import MLTypes


type Table a = Map.Map Name a
type Env = Table Scheme

type TypeTable = Table Int    -- just arity
type ClassTable = Table ClassInfo
type ClassInfo = ([Method], [Inst])
type Method = Name
type Inst = Qual Pred

data TcState = TcState {
 tcsLogEnabled :: Bool,
 tcsLog :: [String],   -- log, reversed
 tcsEnv :: Env,        -- value env
 tcsTT :: TypeTable,
 tcsCT :: ClassTable,
-- tcsIT :: InstTable
 tcsNS :: NS,
 tcsSubst :: Subst,
 constraints :: Constraints
}

initState = TcState
  { tcsLogEnabled = False
  , tcsLog = []
  , tcsEnv = primEnv
  , tcsTT = primTT
  , tcsCT = primCT
  , tcsNS = namePool
  , tcsSubst = emptySubst
  , constraints = []
  }

class ToStr a where
  str :: a -> String

instance {-# OVERLAPPABLE  #-} Show a => ToStr a where str = show
instance {-# OVERLAPPING   #-} ToStr String where str = id
instance {-# OVERLAPPING   #-} ToStr Expr where str = showExpr

instance MonadFail Identity where
  fail = error

type TCM a = ExceptT String (State TcState) a
type T a = TCM a

getEnv :: T Env
getEnv = gets tcsEnv

putEnv :: Env -> T ()
putEnv env = modify (\r -> r { tcsEnv = env })

setLogging :: Bool -> T Bool
setLogging b = do
  old <- gets tcsLogEnabled
  modify (\r -> r { tcsLogEnabled = b })
  pure b

info :: [String] -> T ()
info ss = do
  logging <- gets tcsLogEnabled
  when logging $ modify (\r -> r { tcsLog = concat ss:tcsLog r })

extEnv :: Name -> Scheme -> TCM ()
extEnv n s = do
  env <- gets tcsEnv
  putEnv (Map.insert n s env)

withExtEnv :: Name -> Scheme -> TCM a -> TCM a
withExtEnv n s ta = do
  env <- gets tcsEnv
  putEnv (Map.insert n s env)
  a <- ta
  putEnv env
  pure a

withCurrentSubst :: HasTypes t => t -> TCM t
withCurrentSubst t = do
  s <- gets tcsSubst
  pure (apply s t)

showEnv :: T String
showEnv = do
  env <- gets tcsEnv
  let s = unlines . map show $ Map.toList env
  pure s


askType :: Name -> TCM Scheme
askType n = do
  result <- gets (Map.lookup n . tcsEnv)
  case result of
    Just t -> return t
    Nothing -> error $ "Unknown name: " ++ n

getFreeVars :: TCM [Tyvar]
-- askFreeVars = asks (concatMap (ftv.snd) . Map.toList)
getFreeVars = gets (concatMap (ftv.snd) . Map.toList . tcsEnv)

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
                 -- info ["in unify ", str t1, " ~ ", str t2, show s, " => ", show u]
                 modify(extSubst u)

extSubst   :: Subst -> TcState -> TcState
extSubst s st = st { tcsSubst = s <> s0 } where s0 = tcsSubst st

addConstraint :: Constraint -> TCM ()
addConstraint c = modify (addC c) where
    addC c s = s { constraints = c:constraints s }

freshInst :: Scheme -> TCM (Qual Type)
freshInst (Forall tvs ty) = do
  s <- Subst <$> mapM newTVar tvs
  return (apply s ty)

newTVar :: Tyvar -> TCM (Tyvar, Type)
newTVar tv = do
  n <- tcmDeplete
  return (tv, TVar n)

addMonoBind :: Name -> Type -> Env -> Env
addMonoBind n t = Map.insert n s where
  s = monotype t

addPolyBind :: Name -> Scheme -> Env -> Env
addPolyBind n s = Map.insert n s

runTcS t = runState t initState

primEnv :: Env
primEnv = Map.fromList primVals

primTT :: TypeTable
primTT = Map.fromList primTypes

primCT = Map.fromList primClasses


-- runTCM :: TCM a -> (Either String a, TcState)
runTCM = runTcS . runExceptT

evalTCM :: TCM a -> Either String a
evalTCM = fst. runTCM
