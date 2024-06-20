module TCM where

import NameSupply
import Unify
import Exp(Name,Typ)

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Functor
import qualified Data.Map as Map

type Env = Map.Map Name Typ
emptyEnv = Map.empty


data TcState = TcState 
    { tcsNS :: NS
    , tcsSubst :: Subst
    }

type TCM a = ExceptT String (StateT TcState (Reader Env)) a

initState :: TcState
initState = TcState namePool emptySubst
runTcS :: State TcState a -> (a, TcState)
runTcS t = runState t initState

evalTCM :: TCM a -> Either String a
evalTCM m = runReader (evalStateT (runExceptT m) initState) emptyEnv

runTCM :: TCM a -> (Either String a, TcState)
runTCM m = runReader (runStateT (runExceptT m) initState) emptyEnv

askType :: Name -> TCM Typ
askType n = do
  result <- asks (Map.lookup n)
  case result of
    Just t -> return t
    Nothing -> throwError $ "Unknown name: " ++ n

extEnv :: Name -> Typ -> Env -> Env
extEnv = Map.insert

tcsDeplete :: TcState -> (String,TcState)
tcsDeplete s = (name, s { tcsNS = ns' }) where
  ns = tcsNS s
  (name,ns') = deplete ns

nssDeplete :: Monad m => StateT TcState m String
nssDeplete = StateT $ return . tcsDeplete

tcmDeplete :: TCM String
tcmDeplete = lift nssDeplete

freshName :: TCM Name
freshName = tcmDeplete

getSubst :: TCM Subst
getSubst = gets tcsSubst

unify :: Typ -> Typ -> TCM ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 modify(extSubst u)

extSubst   :: Subst -> TcState -> TcState
extSubst s st = st { tcsSubst = s <> s0 } where s0 = tcsSubst st


applyCurrentSubst :: HasTypes t => t -> TCM t
applyCurrentSubst t = do
  s <- gets tcsSubst
  pure (apply s t)
