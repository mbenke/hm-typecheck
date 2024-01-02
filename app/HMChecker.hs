{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HMChecker where
-- import Prelude hiding(lookup)
import Data.List((\\))
import Data.Maybe(fromJust)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import MLExpr
-- import MLSyntax
import MLTypes
import NameSupply
import MLConstraints
import TCM


tiExpr :: Expr -> TCM ([Pred], Type)
tiExpr (EInt _) = pure ([], TInt)
tiExpr (ELam args e1) = do
  env <- getEnv
  as <- addArgs args
  (ps, t1) <- tiExpr e1
  tas <- withCurrentSubst as
  putEnv env
  pure $ (ps, foldr (:->) t1 tas)

tiExpr (EVar n) = do
  s <- askType (name n)
  ps :=> t <- freshInst s
  pure (ps, t)

tiExpr e@(EApp e1 e2) = do
  (ps, t1) <- tiExpr e1
  (qs, t2) <- tiExpr e2
  freshname <- tcmDeplete
  let tr = TVar freshname
  -- info  ["unify ", show t1, " ~ ", show (t2 :-> tr)]
  unify t1 (t2 :-> tr)
  s <- getSubst
  info ["getSubst: ", show s]
  let tr' = apply s tr
  info ["tiExpr ", str e, " :: ", str tr' ]
  pure (ps ++ qs, tr')

tiExpr exp@(ELet ident e1 e2) = do
  let n = name ident
  (ps, t1) <- tiExpr e1
  info ["tiExpr ", show ps, "|-", str e1, " :: ", str t1 ]
  s <- generalize (ps, t1)
  info ["tiExpr ", str e1, " :: ", str s ]
  (qs, t) <- withExtEnv n s (tiExpr e2)
  pure (ps ++ qs, t)

tiExpr (ERec [] e) = tiExpr e
tiExpr _ = error "ERec not implemented" -- TODO

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
  t <- withCurrentSubst t0
  ps1 <- withCurrentSubst ps0
  ce <- gets tcsCT
  let ps2 = simplify ce ps1
  let typeVars =  ftv t
  let ps = filter (nonTrivial typeVars) ps2
  return $ Forall (typeVars \\ envVars) (ps2 :=> t) -- FIXME: context red


isFreeInEnv :: Tyvar -> TCM Bool
isFreeInEnv tv = do
  env <- gets tcsEnv
  let ets = map snd (Map.toList env)
  let etv = ftv ets
  pure (tv `elem` etv)

typeOf1 :: Expr -> TCM Scheme
typeOf1 exp = catchError ty handler where
  ty = (tiExpr exp) >>= generalize
  -- generalize (ps, t) = Forall [] (ps :=> t)   -- FIXME: add context reduction
  handler :: String -> TCM Scheme
  handler e = do
    envRep <- showEnv
    throwError $
      "Type error in\n  "
      ++showExpr exp++"\n"
      ++e
      -- ++"\nEnv:"
      -- ++envRep


---- Classes

insts :: ClassTable -> Name -> [Inst]
insts ce n = case Map.lookup n ce of Just (ms, is) -> is

getInsts :: Name -> TCM [Inst]
getInsts n = do
  ce <- gets tcsCT
  case Map.lookup n ce of
    Just (ms, is) -> pure is
    Nothing -> throwError$ "unknown class: " ++  n

byInst :: ClassTable -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t)= msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe [Pred]
    tryInst(ps :=> h) = case matchPred h p of
                          Left _ -> Nothing
                          Right u -> Just (map (apply u) ps)

nonTrivial :: [Tyvar] -> Pred -> Bool
nonTrivial tvs (IsIn _ (TVar a)) | not (elem a tvs) = False
nonTrivial tvs _ = True

simplify :: ClassTable -> [Pred] -> [Pred]
simplify ce = loop [] where
    loop rs [] = rs
    loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                   | otherwise = loop (p:rs) ps

entail :: ClassTable -> [Pred] -> Pred -> Bool
entail ce ps p = p `elem` ps ||
                 case byInst ce p of
                   Nothing -> False
                   Just qs -> all (entail ce ps) qs

-- typeOf :: Expr -> Either String Scheme
typeOf exp = do
  let (t,state) = runTCM (typeOf1 exp)
  let subst = tcsSubst state
  apply subst <$> t

typeCheck :: Expr -> IO ()
typeCheck exp = case typeOf exp of
                  Left e -> putStrLn "Error: " >> putStrLn e
                  Right t -> putStrLn $ (showExpr exp) ++ " :: " ++ show t

testCheck :: Expr -> IO ()
testCheck exp = do
  let expS = showExpr exp
  putStrLn $ "Check: " ++ expS
  let (result,state) = runTCM (typeOf1 exp)
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
                    putStr "Substitution:"
                    print subst

{-
solve :: Constraints -> Subst -> TCM Subst
solve [] subst = return subst
solve ((l,r):cs) s = do
  s' <- mgu l r
  solve cs s'
-}

unifError :: Constraint -> TCM a
unifError (t1,t2) = throwError $ "Cannot unify "++show t1++" with "++show t2
