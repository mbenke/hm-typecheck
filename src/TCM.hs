{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
module TCM where
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

import Data.List(intercalate)
import qualified Data.Map as Map
import Data.Maybe(isJust)

import NameSupply
import Constraints
import ISyntax

import Prims(primVals, primTypes, primClasses, eqInstances, refInstances)
import Types


type Table a = Map.Map Name a
type Env = Table Scheme            -- Environment maps names to type schemes


type TypeTable = Table TypeInfo    -- TypeTable maps type constructor names to
type TypeInfo = (Arity, [Name])    -- arity and constructor names

type ClassTable = Table ClassInfo
type ClassInfo = (Arity, [Method]) -- number of weak parameters and method names
type InstTable = Table [Inst]      -- instances list for a given class name
type Arity = Int
type Method = Name

data TcState = TcState {
 tcsLogEnabled :: Bool,
 tcsLog :: [String],               -- log, reversed
 tcsEnv :: Env,                    -- environment
 tcsTT :: TypeTable,               -- type constructors
 tcsCT :: ClassTable,              -- classes
 tcsIT :: InstTable,               -- class instances
 tcsNS :: NS,                      -- fresh name supply
 tcsSubst :: Subst                 -- current substitution, needed for type reconstruction
                                   -- may not be needed when only type checking
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
instance {-# OVERLAPPING   #-} ToStr InstTable where str = unlines . map show . Map.toList
instance MonadFail Identity where
  fail = error

type TCM a = ExceptT String (State TcState) a
type T a = TCM a

getEnv :: T Env
getEnv = gets tcsEnv

putEnv :: Env -> T ()
putEnv env = modify (\r -> r { tcsEnv = env })

setLogging :: Bool -> T Bool
setLogging switch = do
  wasLogging <- gets tcsLogEnabled
  -- warn ["!! setLogging from:", str wasLogging]
  modify (\r -> r { tcsLogEnabled = switch })
  -- warn ["!! setLogging to: ", str switch]
  return wasLogging

withLogging :: T a -> T a
withLogging action = do
  restore <- setLogging True
  result <- action
  setLogging restore
  pure result

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

getCurrentSubst :: TCM Subst
getCurrentSubst = gets tcsSubst

withCurrentSubst :: HasTypes t => t -> TCM t
withCurrentSubst t = do
  s <- gets tcsSubst
  pure (apply s t)

logCurrentSubst :: TCM ()
logCurrentSubst = do
  s <- getCurrentSubst
  info["! subst = ", str s]

showEnv :: Bool -> Env -> String
showEnv withPrims env = concat . map showEntry $ Map.toList env where
    showEntry (n, s)
        | not(isPrimitive n) || withPrims = n ++ " : " ++ str (legibleScheme s) ++ "\n"
        | otherwise = ""
    isPrimitive n = isJust (Map.lookup n primEnv)

showSmallEnv :: [(Name,Scheme)] -> String
showSmallEnv env = intercalate ", " [showEntry (n,s) | (n,s) <- env, not(isPrimitive n)]
    where
    withPrims = False
    showEntry (n, s) = n ++ " : " ++ str (legibleScheme s)
    isPrimitive n = isJust (Map.lookup n primEnv)


askType :: Name -> TCM Scheme
askType n = do
  result <- gets (Map.lookup n . tcsEnv)
  case result of
    Just t -> return t
    Nothing -> throwError $ "Unknown name: " ++ n

askTypes :: [Name] -> TCM [(Name, Scheme)]
askTypes  = mapM ask where ask n = do { t <- askType n; return (n,t) }

getFreeVars :: TCM [Tyvar]
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
                 extSubst u

extSubst   :: Subst -> TCM ()
extSubst s = modify ext where
    ext st = st { tcsSubst = s <> tcsSubst st }

clearSubst :: TCM ()
clearSubst = modify clearS where
    clearS st = st { tcsSubst = emptySubst }

-- Use more legible names for quantified variables
-- such as    forall a  b . a  -> b  -> a
-- instead of forall u3 v1. u3 -> v1 -> u3
legibleScheme :: Scheme -> Scheme
legibleScheme (Forall tvs ty) = Forall tvs' ty' where
    tvs' = take (length tvs) namePool
    subst = Subst $ zip tvs (map TVar tvs')
    ty' = apply subst ty

freshInst :: Scheme -> TCM (Qual Type)
freshInst (Forall tvs ty) = renameVars tvs ty

-- replace all free type variables with fresh names
renameFreeVars :: HasTypes a => a -> TCM a
renameFreeVars a = renameVars (ftv a) a

renameVars :: HasTypes a => [Name] -> a -> TCM a
renameVars tvs a = do
  s <- Subst <$> mapM newTVar tvs
  return (apply s a)

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
