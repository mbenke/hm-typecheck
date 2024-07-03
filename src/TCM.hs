module TCM where
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

import Data.List(intercalate)
import qualified Data.Map as Map
import Data.Maybe(isJust)

import Common.NameSupply
import Language.Fun.Constraints
import Language.Fun.ISyntax

import Language.Fun.Prims(primVals, primTypes, primClasses, eqInstances, refInstances)
import Language.Fun.Types


type Table a = Map.Map Name a
type Env = Table Scheme            -- Environment maps names to type schemes

type TypeTable = Table TypeInfo    -- TypeTable maps type constructor names to
type TypeInfo = (Arity, [ConInfo])    -- arity and constructor names
type ConInfo = (Name, Scheme)      -- constructor name and type scheme

type ClassTable = Table ClassInfo
type ClassInfo = (Arity, [Method]) -- number of weak parameters and method names
type InstTable = Table [Inst]      -- instances list for a given class name
type SpecTable = Table [Specialisation]
type TLDict = Table TLDef
type Arity = Int
type Method = Name
type TLDef = (Name, Scheme, [Arg], TcExpr)
type Specialisation = (Name, Type, [Arg], TcExpr)
type ResolutionEnv = Map.Map Name [Resolution]
type Resolution = (Type, TcExpr)

data TcState = TcState {
 tcsLogEnabled :: Bool,
 tcsLog :: [String],               -- log, reversed
 tcsEnv :: Env,                    -- environment
 tcsTT :: TypeTable,               -- type constructors
 tcsCT :: ClassTable,              -- classes
 tcsIT :: InstTable,               -- class instances
 tcsTLD :: TLDict,                 -- toplevel definitions
 tcsSpec :: SpecTable,             -- monomorphic specialisations
 tcsREnv :: ResolutionEnv,
 tcsNS :: NS,                      -- fresh name supply
 tcsSubst :: Subst,                -- current substitution, needed for type reconstruction
                                   -- may not be needed when only type checking
 tcsCoverageEnabled :: Bool        -- whether to check instances for coverage
}

initState = init TcState
  { tcsLogEnabled = False
  , tcsLog = []
  , tcsEnv = primEnv
  , tcsTT = primTT
  , tcsCT = Map.empty
  , tcsIT = Map.empty
  , tcsTLD = Map.empty
  , tcsSpec = Map.empty
  , tcsREnv = Map.empty
  , tcsNS = namePool
  , tcsSubst = emptySubst
  , tcsCoverageEnabled = True
  } where
    addInstances :: [Inst] -> TcState -> TcState
    addInstances is st = foldr addInstInfo st is
    init :: TcState -> TcState
    init = addPrimClasses

type TCM a = ExceptT String (State TcState) a
type T a = TCM a

maybeToTCM :: String -> Maybe a -> TCM a
maybeToTCM msg Nothing = throwError msg
maybeToTCM _ (Just a) = pure a

wrapError :: ToStr ctxt => TCM a -> ctxt -> TCM a
wrapError m ctxt = catchError m handler where
    handler msg =
        throwError (decorate msg)
    decorate msg = msg ++ "\n  - in " ++ str ctxt

getEnv :: T Env
getEnv = gets tcsEnv

putEnv :: Env -> T ()
putEnv env = modify (\r -> r { tcsEnv = env })

getCoverageCheck :: T Bool
getCoverageCheck = gets tcsCoverageEnabled

setCoverageCheck :: Bool -> T Bool
setCoverageCheck switch = do
  wasChecking <- gets tcsCoverageEnabled
  modify (\r -> r { tcsCoverageEnabled = switch })
  return wasChecking

setLogging :: Bool -> T Bool
setLogging switch = do
  wasLogging <- gets tcsLogEnabled
  modify (\r -> r { tcsLogEnabled = switch })
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
withExtEnv n s ta = withLocalEnv (extEnv n s >> ta)

withLocalEnv :: TCM a -> TCM a
withLocalEnv ta = do
  env <- gets tcsEnv
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
  info ["! subst = ", str s]

showEnv :: Bool -> Env -> String
showEnv withPrims env = concatMap showEntry $ Map.toList env where
    showEntry (n, s)
        | not (nameIsPrimitive n) || withPrims = n ++ " : " ++ str (legibleScheme s) ++ "\n"
        | otherwise = ""

showSpecTable :: SpecTable -> String
showSpecTable table = unlines . map showSpecs $ Map.toList table where
    showSpecs :: (Name, [Specialisation]) -> String
    showSpecs (n, specs) = intercalate "\n" (map showEntry specs)
    showEntry :: Specialisation -> String
    showEntry (name, typ, args, body) = showDecl (ValBind name args body)

showREnv :: ResolutionEnv -> String
showREnv = unlines . map showRes . Map.toList where
    showRes :: (Name, [Resolution]) -> String
    showRes (n, res) = intercalate "\n"  (map showEntry res) where
      showEntry :: Resolution -> String
      showEntry (t, e) = n ++ " : " ++ str t ++ " = " ++ show e

nameIsPrimitive n = isJust (Map.lookup n primEnv)

showSmallEnv :: [(Name,Scheme)] -> String
showSmallEnv env = intercalate ", " [showEntry (n,s) | (n,s) <- env, not (nameIsPrimitive n)]
    where
    withPrims = False
    showEntry (n, s) = n ++ " : " ++ str (legibleScheme s)

maybeAskType :: Name -> TCM (Maybe Scheme)
maybeAskType n = gets (Map.lookup n . tcsEnv)

askType :: Name -> TCM Scheme
askType n = do
  result <- maybeAskType n
  case result of
    Just t -> return t
    Nothing -> throwError $ "Unknown name: " ++ n

askTypes :: [Name] -> TCM [(Name, Scheme)]
askTypes  = mapM ask where ask n = do { t <- askType n; return (n,t) }

getFreeEnvVars :: TCM [Tyvar]
getFreeEnvVars = gets (concatMap (ftv.snd) . Map.toList . tcsEnv)

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

-- |`pruneSubst` removes given type variables from current substitution
pruneSubst :: [Tyvar] -> TCM ()
pruneSubst tvs = modify (\st -> st { tcsSubst = expel tvs (tcsSubst st) })

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


addTypeInfo :: Name -> TypeInfo -> TCM ()
addTypeInfo name typeInfo = modifyTypeTable (Map.insert name typeInfo)

addClassInfo :: Name -> ClassInfo -> TcState -> TcState
addClassInfo n ci st = st { tcsCT = extCT (tcsCT st), tcsIT = extIT (tcsIT st) } where
    extCT = Map.insert n ci
    extIT = Map.insert n []

addInstInfo :: Inst -> TcState -> TcState
addInstInfo inst@(ctx :=> InCls name _ _) st = st { tcsIT = ext (tcsIT st) } where
    ext = Map.insertWith (++) name [inst]

-- addInstsFromClassInfo :: (Name, ClassInfo) -> TcState -> TcState

lookupTLD :: Name -> TCM (Maybe TLDef)
lookupTLD name = gets (Map.lookup name . tcsTLD)

lookupSpec :: Name -> Type ->  TCM (Maybe Specialisation)
lookupSpec name typ = do
  res <- gets (Map.lookup name . tcsSpec)
  return (res >>= find typ)
  where
    find typ [] = Nothing
    find typ ((n, t, args, body):specs)
      | typ == t = Just (n, t, args, body)
      | otherwise = find typ specs

getTypeInfo :: Name -> TCM TypeInfo
getTypeInfo name = lookupTypeInfo name >>= maybe report return where
  report = throwError $ "Unknown type constructor: " ++ name

lookupTypeInfo :: Name -> TCM (Maybe TypeInfo)
lookupTypeInfo name = gets (Map.lookup name . tcsTT)

getConstructorsFor :: Name -> TCM [ConInfo]
getConstructorsFor name = do
  (arity, cons) <- getTypeInfo name
  pure cons

getConInfo :: Name -> TCM ConInfo
getConInfo name = do
  s <- askType name
  return (name, s)

addSpecialisation :: Name -> Type -> [Arg] -> TcExpr -> TCM ()
addSpecialisation name typ args body = do
  mspec <- lookupSpec name typ
  case mspec of
    Just _ -> return ()
    Nothing -> modify extSpec where
      extSpec st = st { tcsSpec = addSpec (tcsSpec st)}
      addSpec = Map.insertWith (++) name [(name, typ, args, body)]

addResolution :: Name -> Type -> TcExpr -> TCM ()
addResolution name typ expr = modify ext where
    ext st = st { tcsREnv = Map.insertWith (++) name [(typ, expr)] (tcsREnv st) }

{- | lookupResolution name typ
   looks up the resolutions for name in the resolution environment
   and returns the first resolution that matches the type along with the substitution;
   since overlapping instances are forbidden, there can be at most one matching resolution
-}
lookupResolution :: Name -> Type -> TCM(Maybe (TcExpr, Type, Subst))
lookupResolution name etyp = gets (Map.lookup name . tcsREnv) >>= findMatch etyp where
  findMatch :: Type -> Maybe [Resolution] -> TCM (Maybe (TcExpr, Type, Subst))
  findMatch etyp (Just res) = firstMatch etyp res
  findMatch _ Nothing = return Nothing
  firstMatch :: Type -> [Resolution] -> TCM (Maybe (TcExpr, Type, Subst))
  firstMatch etyp [] = return Nothing
  firstMatch etyp ((t,e):rest)
    | Right subst <- mgu t etyp = do  -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
        warn ["! lookupRes: match found: ", str t, " ~ ", str etyp, " => ", str subst]
        return (Just (e, t, subst))
    | otherwise = firstMatch etyp rest

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

runTCM :: TCM a -> (Either String a, TcState)
runTCM = runTcS . runExceptT

evalTCM :: TCM a -> Either String a
evalTCM = fst. runTCM

getTypeTable :: TCM TypeTable
getTypeTable = gets tcsTT

modifyTypeTable :: (TypeTable -> TypeTable) -> TCM ()
modifyTypeTable f = modify (\st -> st { tcsTT = f (tcsTT st) })

-- find type to which a given data constructor belongs
lookupCon :: Name -> TypeTable -> (Name, [ConInfo])
lookupCon con tt = go (Map.toList tt) where
    go [] = error ("lookupCon: unknown constructor "++str con)
    go ((tname, (arity, cis)):ts) =
      if con `elemByFst` cis then (tname, cis) else go ts
    elemByFst x ys = any ((==x).fst) ys

lookupConSiblings :: Name -> TypeTable -> [ConInfo]
lookupConSiblings con tt = snd (lookupCon con tt)

lookupConType :: Name -> TypeTable -> Name
lookupConType con tt = fst (lookupCon con tt)
