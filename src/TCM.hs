{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
module TCM where
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

import Data.List(foldl')
import qualified Data.Map as Map
import Data.Maybe(isJust)

import NameSupply
import Constraints
import Syntax

import Prims(primVals, primTypes, primClasses, eqInstances, refInstances)
import Types


type Table a = Map.Map Name a
type Env = Table Scheme


type TypeTable = Table TypeInfo
type TypeInfo = (Arity, [Name])  -- arity and oconstructor names
type ClassTable = Table ClassInfo
type ClassInfo = (Arity, [Method])
type InstTable = Table [Inst]
type Arity = Int
type Method = Name

data TcState = TcState {
 tcsLogEnabled :: Bool,
 tcsLog :: [String],   -- log, reversed
 tcsEnv :: Env,        -- value env
 tcsTT :: TypeTable,
 tcsCT :: ClassTable,
 tcsIT :: InstTable,
 tcsNS :: NS,
 tcsSubst :: Subst,
 constraints :: Constraints
}

initState = init TcState
  { tcsLogEnabled = False
  , tcsLog = []
  , tcsEnv = primEnv
  , tcsTT = primTT
  , tcsCT = Map.empty
  , tcsIT = Map.empty
  , tcsNS = namePool
  , tcsSubst = emptySubst
  , constraints = []
  } where
    addInstances :: [Inst] -> TcState -> TcState
    addInstances is st = foldr addInstInfo st is
    init :: TcState -> TcState
    init = addPrimClasses
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

warn :: [String] -> T ()
warn ss = do modify (\r -> r { tcsLog = concat ss:tcsLog r })

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

showEnv :: Bool -> Env -> String
showEnv withPrims env = concat . map showEntry $ Map.toList env where
    showEntry (n, s)
        | not(isPrimitive n) || withPrims = n ++ " : " ++ str s ++ "\n"
        | otherwise = ""
    isPrimitive n = isJust (Map.lookup n primEnv)

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

addTypeInfo :: Name -> TypeInfo -> TcState -> TcState
addTypeInfo name typeInfo state = state { tcsTT = ext (tcsTT state) } where
  ext = Map.insert name typeInfo

addClassInfo :: Name -> ClassInfo -> TcState -> TcState
addClassInfo n ci st = st { tcsCT = extCT (tcsCT st), tcsIT = extIT (tcsIT st) } where
    extCT = Map.insert n ci
    extIT = Map.insert n []

addInstInfo :: Inst -> TcState -> TcState
addInstInfo inst@(ctx :=> InCls name _ _) st = st { tcsIT = ext (tcsIT st) } where
    ext = Map.insertWith (++) name [inst]

-- addInstsFromClassInfo :: (Name, ClassInfo) -> TcState -> TcState

runTcS t = runState t initState

primEnv :: Env
primEnv = Map.fromList primVals

primTT :: TypeTable
primTT = Map.fromList primTypes

primCT = Map.fromList primClasses

addPrimClasses :: TcState -> TcState
addPrimClasses st = foldr addC st primClasses where
    addC :: (Name, ClassInfo) -> TcState -> TcState
    addC  = uncurry addClassInfo

-- runTCM :: TCM a -> (Either String a, TcState)
runTCM = runTcS . runExceptT

evalTCM :: TCM a -> Either String a
evalTCM = fst. runTCM
