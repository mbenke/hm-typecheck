{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HMChecker where
-- import Prelude hiding(lookup)
import Data.List((\\))
-- import Data.Maybe(fromJust)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import MLExpr
-- import MLSyntax
import MLTypes
import NameSupply
import MLConstraints
import TCM
import Debug


tiExpr :: Expr -> TCM ([Pred], Type)
tiExpr (EInt _) = pure ([], TInt)
tiExpr (ELam args e1) = do
  env <- getEnv
  as <- addArgs args
  (ps, t1) <- tiExpr e1
  tas <- withCurrentSubst as
  putEnv env
  pure $ (ps, foldr (:->) t1 tas)

tiExpr (EVar v) = do
  s <- askType (name v)
  info ["var ", name v, " :: ", str s]
  ps :=> t <- freshInst s
  info ["var ", str ps, " |- ", name v, " :: ", str t]
  pure (ps, t)

tiExpr e@(EApp e1 e2) = do
  setLogging False
  (ps, t1) <- tiExpr e1
  (qs, t2) <- tiExpr e2
  freshname <- tcmDeplete
  let tr = TVar freshname
  -- info  ["unify ", show t1, " ~ ", show (t2 :-> tr)]
  unify t1 (t2 :-> tr) `wrapError` e
  s <- getSubst
  -- info ["getSubst: ", show s]
  let tr' = apply s tr
  let preds = apply s (ps ++ qs)
  info ["tiExpr ", str e, " :: ", str tr' ]
  pure (preds, tr')

tiExpr exp@(ELet ident e1 e2) = do
  let n = name ident
  s <- tiBind n [] e1
  restore <- setLogging True
  info ["tiExpr let ", n, " :: ", str s ]
  setLogging restore
  (qs, t) <- withExtEnv n s (tiExpr e2)
  pure (qs, t)

tiExpr (ERec [] e) = tiExpr e
tiExpr _ = error "ERec not implemented" -- TODO

tiBind :: Name -> [Arg] -> Expr -> TCM Scheme
tiBind n args e = do
  env <- getEnv
  as <- addArgs args
  (ps, t0) <- tiExpr e
  info ["tiBind ", show ps, " |- ", str e, " :: ", str t0 ]
  tas <- withCurrentSubst as
  putEnv env
  generalize (ps, foldr (:->) t0 tas)

tiArg :: Arg -> TCM (Name, Type)
tiArg (UArg ident) = do
  a <- tcmDeplete
  pure (name ident, TVar a)

addArgs :: [Arg] -> TCM [Type]
addArgs args = do
  typedArgs <- forM args tiArg
  forM typedArgs $ \(n,t) -> extEnv n (monotype t) >> pure t

generalize :: ([Pred], Type) -> TCM Scheme
generalize (ps0, t0) = do
  envVars <- getFreeVars
  (ps1, t) <- withCurrentSubst (ps0, t0)
  ce <- gets tcsCT
  (ps2, phi) <- simplifyM ps1
  let t2 = apply phi t
  let typeVars =  ftv t2
  let ps = filter (nonTrivial typeVars) ps2
  return $ Forall (typeVars \\ envVars) (ps2 :=> t2)


isFreeInEnv :: Tyvar -> TCM Bool
isFreeInEnv tv = do
  env <- gets tcsEnv
  let ets = map snd (Map.toList env)
  let etv = ftv ets
  pure (tv `elem` etv)

schemeOf :: Expr -> TCM Scheme
schemeOf exp = wrapError ty exp where
  ty = (tiExpr exp) >>= generalize

wrapError :: ToStr ctxt => TCM a -> ctxt -> TCM a
wrapError m ctxt = catchError m handler where
    handler msg =
        throwError (decorate msg)
    decorate msg = msg ++ "\n  - in " ++ str ctxt

tiDecl :: Decl -> TCM Scheme
tiDecl (ValDecl i ct) = do
  let t = desugarT ct
  let tvs = ftv t
  let s = Forall tvs ([] :=> t)
  extEnv (name i) s
  pure s
tiDecl (ValBind i as e) = do
  let n = name i
  s <- tiBind n as e `wrapError` n
  extEnv n s
  pure s


tiProg :: Prog -> TCM ()
tiProg (Prog decls) = mapM_ tiDecl decls

---- Classes

insts :: InstTable -> Name -> [Inst]
insts ce n = Map.findWithDefault (error ("instance " ++ n ++ " not found")) n ce

getInsts :: Name -> TCM [Inst]
getInsts n = do
  ce <- gets tcsIT
  case Map.lookup n ce of
    Just is -> pure is
    Nothing -> throwError$ "unknown class: " ++  n

{-
byInst :: ClassTable -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t)= msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe [Pred]
    tryInst(ps :=> h) = case matchPred h p of
                          Left _ -> Nothing
                          Right u -> Just (apply u ps)
byInst ce p@(InCls i as t)= msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe [Pred]
    tryInst(ps :=> h) = case mguPred h p of
                          Left _ -> Nothing
                          Right u -> Just (map (apply u) ps)
-}

nonTrivial :: [Tyvar] -> Pred -> Bool
nonTrivial tvs (IsIn _ (TVar a)) | not (elem a tvs) = False
nonTrivial tvs _ = True

{-
simplify :: ClassTable -> [Pred] -> TCM [Pred]
simplify ce ps = do
  ce <- gets tcsCT
  loop [] ps where
    loop rs [] = pure rs
    loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                   | otherwise = loop (p:rs) ps

entail :: ClassTable -> [Pred] -> Pred -> Bool
entail ce ps p = p `elem` ps || maybe False allAssumptions (byInst ce p) where
    allAssumptions = all (entail ce ps)
-}
simplifyM :: [Pred] -> TCM([Pred], Subst)
simplifyM ps = do
  setLogging (length ps > 0)
  ce <- gets tcsIT
  info ["> simplifyM ", str ps]
  loop ce [] ps emptySubst where
    loop :: InstTable -> [Pred] -> [Pred] -> Subst -> TCM([Pred], Subst)
    loop ce rs [] subst = do
                 info ["< simplifyM ", str ps, " = ", str (rs,subst)]
                 pure (rs, subst)
    loop ce rs (p:ps) subst
             | elem p ps = loop ce rs ps subst
             | otherwise = case entailM ce (rs++ps) p of
                 Just phi -> do
                             info ["< entailM ", str ps, " |- ", str p, " = ", show phi]
                             loop ce rs ps (phi <> subst)
                 Nothing -> loop ce (p:rs) ps subst

entailM :: InstTable -> [Pred] -> Pred -> Maybe Subst
entailM ce ps (t :~: u) = maybeFromRight (mgu t u)
entailM ce ps p = case elem p ps of
                    True -> Just emptySubst
                    False -> do
                      (qs, u) <- byInstM ce p
                      case qs of
                        [] -> Just u
                        [q] -> do
                                u' <- entailM ce ps (apply u q)
                                pure (u <> u')

                        _ -> error("Unimplemented - Complex instance context " ++ show (qs, u))

byInstM :: InstTable -> Pred -> Maybe ([Pred], Subst)
byInstM ce p@(InCls i as t) = msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe ([Pred], Subst)
    tryInst c@(ps :=> h) = trace (unwords["!> tryInst", str c, "for", str p]) $
        case matchPred h p of
          Left _ -> trace(unwords["!< matchPred", str h, "<~", str p,"FAIL"])Nothing
          Right u -> let tvs = ftv h
                     in trace(unwords["!< matchPred", str h, "<~", str p,"=",str u])
                        Just (map (apply u) ps, expel tvs u)




-- typeOf :: Expr -> Either String Scheme
typeOf exp = do
  let (t,state) = runTCM (schemeOf exp)
  let subst = tcsSubst state
  apply subst <$> t

typeCheck :: Expr -> IO ()
typeCheck exp = case evalTCM (schemeOf exp) of
                  Left e -> putStrLn "Error: " >> putStrLn e >> putStrLn ""
                  Right t -> putStrLn $ (showExpr exp) ++ " :: " ++ show t

verboseCheck :: Expr -> IO ()
verboseCheck exp = do
  let expS = showExpr exp
  putStrLn $ "Check: " ++ expS
  let (result,state) = runTCM (setLogging True >> schemeOf exp)
  let  cons = constraints state
  let  subst = tcsSubst state
  let history = reverse (tcsLog state)
  putStrLn "History:"
  mapM_ putStrLn history
  putStrLn "------------------------------------------------------------------------"
  case result of
                  Left e -> do
                    putStrLn "Error: "
                    putStrLn e
                    putStrLn "Constraints:"
                    print cons
                  Right t -> do
                    putStrLn expS
                    let typ = apply subst t
                    putStr ":: "
                    print typ
                    -- putStrLn "Constraints:"
                    -- print cons
                    -- putStr "Substitution:"
                    -- print subst

{-
solve :: Constraints -> Subst -> TCM Subst
solve [] subst = return subst
solve ((l,r):cs) s = do
  s' <- mgu l r
  solve cs s'
-}

unifError :: Constraint -> TCM a
unifError (t1,t2) = throwError $ "Cannot unify "++show t1++" with "++show t2

maybeFromRight :: Either a b -> Maybe b
maybeFromRight = either (const Nothing) Just
