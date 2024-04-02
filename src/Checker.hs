{-# LANGUAGE FlexibleInstances #-}
module Checker where
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List((\\), nub, intercalate)
import qualified Data.Map as Map

import ISyntax
import Types
import NameSupply
import Constraints
import TCM
import Debug


tiExpr :: Expr -> TCM ([Pred], Type)
tiExpr (EInt _) = pure ([], TInt)
-- check a lambda (function) such as `\x y -> x`
tiExpr (ELam args e1) = do
  env <- getEnv
  as <- addArgs args
  (ps, t1) <- tiExpr e1
  -- typing function body may have created constraints on argument types
  -- these are recorded in the current substitution, which needs to be applied here
  tas <- withCurrentSubst as
  putEnv env
  pure $ (ps, foldr (:->) t1 tas)

tiExpr (EVar v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (ps, t)

tiExpr (ECon v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (ps, t)

-- check an application (function call)
tiExpr e@(EApp fun arg) = do
  (ps, t1) <- tiExpr fun
  -- info ["! EApp fun: ", str(ps :=> t1)]
  (qs, t2) <- tiExpr arg
  -- info ["! EApp arg: ", str(qs :=> t2)]
  freshname <- tcmDeplete
  let tr = TVar freshname
  unify t1 (t2 :-> tr) `wrapError` e
  s <- getSubst
  let tr' = apply s tr
  let preds = apply s (ps ++ qs)
  -- info ["! EApp res:  ", str e, " :: ", str (preds :=> tr') ]
  pure (preds, tr')

tiExpr exp@(ELet x e1 e2) = do
  s <- tiBind x [] e1
  -- info ["tiExpr let ", x, " :: ", str s ]
  (qs, t) <- withExtEnv x s (tiExpr e2)
  pure (qs, t)

tiExpr (ETyped e t) = do
  (ps, t1) <- tiExpr e
  unify t t1
  return (ps, t1)

tiExpr (EBlock stmts) = do
    env <- getEnv
    result <- go stmts
    putEnv env
    return result
  where
    unit = ([], unitT)
    go [] = return unit
    go [stmt] = tiStmt stmt
    go (stmt:rest) = do -- tiStmt stmt >> go rest
      tiStmt stmt
      go rest
    tiStmt :: ToStr ann => Stmt ann -> TCM ([Pred], Type)
    tiStmt (SExpr ann e) = do
      localEnv <- askTypes (freeVars e)
      warn [str ann, " ~> ", str e]
      (ps, t) <- tiExpr e `wrapError` e
      Forall as (ps1 :=> t1) <- (generalize (ps, t) `wrapError` e)
      info [showSmallEnv localEnv, " |- ", str e, " : ", str $ ps1 :=> t1]
      return unit

    tiStmt (SAlloc ann n t) = do
      extEnv n (monotype $ stackT t)
      q <- askType n
      warn ["alloc ", n, " : ", str t, " ~> ", n, " : ", str q]
      return unit

------------------------------------------------------------

tiBind :: Name -> [Arg] -> Expr -> TCM Scheme
tiBind n args e = do
  env <- getEnv
  as <- addArgs args
  (ps0, t0) <- tiExpr e
  let t1 = foldr (:->) t0 as
  t <- withCurrentSubst t1
  ps <- withCurrentSubst ps0
  info ["! tiBind ", show ps, " |- ", str e, " :: ", str t ]
  putEnv env
  -- clearSubst
  generalize (ps, t)

tiArg :: Arg -> TCM (Name, Type)
tiArg (UArg name) = do
  a <- tcmDeplete
  pure (name, TVar a)
tiArg (TArg name t) = pure (name, t)

addArgs :: [Arg] -> TCM [Type]
addArgs args = do
  typedArgs <- forM args tiArg
  forM typedArgs $ \(n,t) -> extEnv n (monotype t) >> pure t

generalize :: ([Pred], Type) -> TCM Scheme
generalize (ps0, t0) = do
  env <- getEnv
  info ["> generalize", str (ps0, t0)]
  envVars <- getFreeVars
  -- withLogging $ info ["< getFreeVars", str envVars]
  (ps1, t1) <- withCurrentSubst (ps0, t0)
  -- withLogging $ info ["< withCurrentSubst ", str (ps1 :=> t1)]
  ce <- gets tcsCT
  ps2 <- reduceContext ps1
  phi <- getCurrentSubst
  info ["< reduceContext", str ps2, " subst=",str phi]
  let t2 = apply phi t1
  let typeVars =  ftv (ps2, t2)
  let scheme = Forall (typeVars \\ envVars) (ps2 :=> apply phi t1)
  info ["< generalize: ", str (legibleScheme scheme)]
  return scheme


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

tiDecl :: Decl -> TCM ()
tiDecl (ValDecl n qt) = do
  let tvs = ftv qt
  let s = Forall tvs qt
  extEnv n s

tiDecl (ValBind n as e) = do
  info ["\nChecking ", n]
  s <- tiBind n as e `wrapError` n
  extEnv n s

-- check type declaration,such as `Option a = None |  Some a`
tiDecl (TypeDecl typ@(TCon name args) alts) = do
  constructors <- tiConAlts typ alts
  forM constructors addCon
  let consNames = map fst constructors
  let arity = length args
  let typeInfo = (arity, consNames)
  modify (addTypeInfo name typeInfo)
  where
      addCon (name, typ) = extEnv name typ

-- check instance declaration such as `instance Int : Eq`
tiDecl (InstDecl qp methods) = tiInstance qp methods

-- check class declaration such ass `class a:Eq { eq : a -> a -> Bool }`
tiDecl(ClsDecl pred methods) = do
  methodNames <- forM methods tiMD
  let classInfo = (className, methodNames)
  modify (addClassInfo className (classArity, methodNames)) where
    className = predName pred
    classArity = length $ predArgs pred
    tiMD (ValDecl name ([] :=> typ)) = do
      extEnv name $ Forall (ftv typ) ([pred] :=> typ)
      return name
    tiMD (ValDecl name (preds :=> typ)) = throwError $ unlines
           [ "- in the declaration of class" ++ predName pred
           , "- in method " ++ name ++ ":"
           , "  qualifiers not allowed in method types"
           ]

    tiMD decl = throwError $ unlines
           [ "- in the declaration of class " ++ className ++ ":"
           , "  only type declarations allowed, illegal declaration: " ++ show decl
           ]

tiDecl (Pragma prag) = process prag where
    process "log" = void $ setLogging True >> warn ["-- Logging ON  --"]
    process "nolog" = setLogging False >> warn ["-- Logging OFF --"]

tiConAlts :: Type -> [ConAlt] -> TCM [(Name, Scheme)]
tiConAlts typ alts = forM alts (tiConAlt typ)

-- check a constructor alternative such as `Some a`
-- and within definition of `Option a`, give it type `forall a.a -> Option a`
tiConAlt :: Type -> ConAlt -> TCM (Name, Scheme)
tiConAlt result (ConAlt cname argumentTypes) = pure (cname, simpleGen constructorType) where
  constructorType = foldr (:->) result argumentTypes  -- at1 :-> at2 :-> ... :-> result
  simpleGen :: Type -> Scheme
  simpleGen t = Forall (ftv t) ([] :=> t)

tiInstance :: Qual Pred -> [Decl] -> TCM ()
tiInstance inst methods = do
  -- Type variables in an instance declaration are implicitly bound
  -- so they need to be renamed before the overlap check
  inst'@(q :=> ihead@(InCls c as t)) <- renameFreeVars inst
  warn ["+ tiInstance ", str inst']
  ois <- getInsts c
  checkOverlap t ois
  forM_ methods (checkMethod ihead)
  let anf = anfInstance inst
  modify (addInstInfo anf)
  where
    checkOverlap :: Type -> [Inst] -> TCM ()
    checkOverlap t [] = pure ()
    checkOverlap t (oi@(_ :=> InCls _ _ u):is) = case mgu t u of
      Right s -> throwError
                 (unwords ["instance",str inst,"overlaps", str oi])
      Left _ -> checkOverlap t is
    matchesType t (_ :=> InCls _ _ u) = match t u
    formLambda [] body = body
    formLambda as body = ELam as body
    findPred :: Name -> [Pred] -> Maybe Pred
    findPred cname (p:ps) | predName p == cname = Just p
                          | otherwise = findPred cname ps
    findPred cname [] = Nothing
    checkMethod :: Pred -> Decl -> TCM ()
    checkMethod ihead (ValBind name args body) = do
      let InCls cname cargs mainType = ihead
      Forall tvs (qs :=> genType)  <- askType name
      p <- maybeToTCM ("Constraint for "++cname++ " not found in type of "++name)
                      (findPred cname qs)
      subst <- liftEither (matchPred p ihead)
      -- warn["-- mgu=",str subst]
      let expType = apply subst genType
      let exp = formLambda (apply subst args) body
      let iTypes = apply subst (map TVar tvs)
      let name' = specName  name iTypes
      warn ["- checkMethod ", str (ValBind name' [] exp), " : ", str expType]
      (iq, it) <- tiExpr exp
      warn ["< tiExpr ", str exp, " : ", str (iq:=>it)]
      match it expType
      addResolution name expType (EVar name')
      addSpecialisation name' expType [] exp
      return ()

-- liftEither :: MonadError e m => Either e a -> m a
-- liftEither m = catchError m throwError

tiProg :: Prog -> TCM ()
tiProg (Prog decls) = do
  cleanTiDecls decls
  -- Now all toplevel functions have types in the environment
  tld <- buildTLD decls
  case Map.lookup entrypoint tld of
    Nothing -> return ()
    Just def -> withLogging $ specialiseEntry entrypoint
  where
    cleanTiDecls = mapM_ cleanTiDecl
    cleanTiDecl d = clearSubst >> tiDecl d
    entrypoint = "main"

buildTLD :: [Decl] -> TCM TLDict
buildTLD decls = do
  tld <- foldM addTLD Map.empty decls
  modify (\st -> st { tcsTLD = tld })
  return tld
   where
    addTLD :: TLDict -> Decl -> TCM TLDict
    addTLD tld (ValBind name args body) = do
      scheme@(Forall tvs (ps :=> typ)) <- askType name
      let typedArgs = attachTypes args (argTypes typ)
      let def = (name, scheme, typedArgs, body)
      return (Map.insert name def tld)
    addTLD tld _ = return tld

attachTypes :: [Arg] -> [Type] -> [Arg]
attachTypes = zipWith typedArg where
    typedArg (UArg name) t   = TArg name t
    typedArg (TArg name _) t = TArg name t

lookupTLD :: Name -> TCM (Maybe TLDef)
lookupTLD name = gets (Map.lookup name . tcsTLD)

{-
specialiseEntries :: [Decl] -> [Name] -> TCM ()
specialiseEntries decls entrypoints = do
  tld <- buildTLD decls
  forM_ entrypoints specialiseEntry
-}

specialiseEntry :: Name -> TCM ()
specialiseEntry name = do
  mdef <- lookupTLD name
  def@(name, scheme, args, body) <- maybeToTCM  ("No definition of " ++ name) mdef
  typ <- maybeToTCM ("Type of entry "++name++" is not monomorphic")
         (typeOfScheme scheme)
  warn ["! specialiseEntry ", name, " : ", str typ]
  body' <- specialiseBody args body typ
  info ["! new body: ", str body']
  let newDef = ValBind name args body'
  info ["! new def: ", str newDef]
  addSpecialisation name typ args body'
  return ()

specialiseBody :: [Arg] -> Expr -> Type -> TCM Expr
specialiseBody args exp etyp = do
  env <- getEnv
  as <- addArgs args -- FIXME
  newBody <- specialiseExp exp (resultType etyp args)
  putEnv env
  return newBody

resultType :: Type -> [Arg] -> Type
resultType t [] = t
resultType (ta :-> tr) (a:as) = resultType tr as
resultType typ args = error (
  "resultType: wrong number of arguments; typ="++str typ++"++args="++str args)
specialiseExp :: Expr -> Type -> TCM Expr
specialiseExp e@(EInt i) etyp = pure e
specialiseExp e@(EVar n) etyp = do
  mres <- lookupResolution n etyp
  case mres of
    Just exp -> do
              warn ["! specVar ", n, " resolution: ", str exp]
              return exp
    Nothing -> do
              warn ["! specVar ", n, " to ", str etyp, "- NO resolution: "]
              specialiseFun n etyp

specialiseExp e@(ECon n) etyp = specialiseCon n etyp

specialiseExp e@(EApp fun a) etyp = do
  (fps, ftyp) <- tiExpr fun
  (aps, atyp) <- tiExpr a
  warn ["! specApp ", str e,
        " fun = ", str fun," : ", str ftyp,
        "; arg = ", str a, " : ", str atyp,
        "; target: ", str etyp
       ]

  warn ["> mgu: ",str ftyp ," ~ ", str (atyp :-> etyp)]
  phi <- mgu ftyp (atyp :-> etyp)
  warn ["< mgu: phi=", str phi]
  unify ftyp (atyp :-> etyp)
  atyp' <- withCurrentSubst atyp
  ftyp' <- withCurrentSubst ftyp
  warn ["! specApp - fun: ",str ftyp'," arg: ", str atyp']
  a' <- specialiseExp a atyp'
  f' <- specialiseExp fun ftyp'
  return (EApp f' a')

specialiseExp e etyp = do
  warn ["FAILED to specialise ", str e]
  return e

specialiseFun :: Name -> Type -> TCM Expr
specialiseFun f etyp = do
  mdef <- lookupTLD f
  case mdef of
    Nothing -> return (EVar f) -- it's a local var
    Just def@(_, fscheme, args, body) -> do
      let (Forall tvs (fps :=> ftyp)) = fscheme
      phi <- mgu ftyp etyp
      let tvs' = apply phi (map TVar tvs)
      let f' = specName f tvs'
      warn ["! specFun ",f," : ", str fscheme, " to ", f', " : ", str etyp]
      addResolution f etyp (EVar f')
      def'@(name', typ', args', body') <- specialiseDef  f' def etyp
      let newDef = ValBind name' args' body'
      warn ["! specFun new def: ", show newDef]
      addSpecialisation name' etyp args' body'
      return (EVar f')

specialiseDef :: Name -> TLDef -> Type -> TCM TLDef
specialiseDef name' (_, _, args, body) etyp = do
  let args' = attachTypes args (argTypes etyp)
  let scheme' = monotype etyp
  let etyp' = resultType etyp args
  env <- getEnv
  as <- addArgs args'
  body' <- specialiseExp body etyp'
  putEnv env
  return (name', scheme', args', body')

specialiseCon :: Name -> Type -> TCM Expr
specialiseCon f t = do
  fscheme@(Forall tvs (fps :=> ftyp)) <- askType f
  phi <- mgu ftyp t
  let tvs' = apply phi (map TVar tvs)
  let f' = specName f tvs'
  warn ["! specCon ",f," : ", str fscheme, " to ", f', " : ", str t]
  return (ECon f')

specName :: Name -> [Type] -> Name
specName n [] = n
specName n ts = n ++ "<" ++ intercalate "," (map str ts) ++ ">" -- FIXME

typeOfScheme :: Scheme -> Maybe Type
typeOfScheme (Forall [] ([] :=> t)) = Just t
typeOfScheme _ = Nothing

maybeToTCM :: String -> Maybe a -> TCM a
maybeToTCM msg Nothing = throwError msg
maybeToTCM _ (Just a) = pure a

---- Classes

-- Administraive Normal Form of an MPTC instance:
-- transform `instance C [as] t` into `[bs ~ as] => C bs t`
anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c [] t)) = inst
anfInstance inst@(q :=> p@(InCls c as t)) = q ++ q' :=> InCls c bs t  where
    q' = zipWith (:~:) bs as
    bs = map TVar $ take (length as) freshNames
    tvs = ftv inst
    freshNames = filter (not . flip elem tvs) namePool

insts :: InstTable -> Name -> [Inst]
insts ce n = Map.findWithDefault (error msg) n ce where
  msg = "Internal error: instance " ++ n ++ " not found"

getInsts :: Name -> TCM [Inst]
getInsts n = do
  ce <- gets tcsIT
  case Map.lookup n ce of
    Just is -> pure is
    Nothing -> throwError$ "unknown class: " ++  n

-- Transform equality constraints (such as a ~ Memory[Int]) into substitution
-- so that [b1:Ref[Int],b1 ~ Memory[Int]] becomes ([Memory[Int]:Ref[Int], b1 +-> Int)
simplifyEqualities :: [Pred] -> TCM [Pred]
simplifyEqualities ps = go [] ps where
    go rs [] = return rs
    go rs ((t :~: u):ps) = do
      phi <- mgu t u
      extSubst phi
      ps' <- withCurrentSubst ps
      rs' <- withCurrentSubst rs
      go rs' ps'
    go rs (p:ps) = go (p:rs) ps

byInstM :: InstTable -> Pred -> Maybe ([Pred], Subst)
byInstM ce p@(InCls i as t) = msum [tryInst it | it <- insts ce i] where
    tryInst :: Qual Pred -> Maybe ([Pred], Subst)
    tryInst c@(ps :=> h) =
        case matchPred h p of
          Left _ -> Nothing
          Right u -> let tvs = ftv h
                     in  Just (map (apply u) ps, expel tvs u)

-- Reducing contexts to hnf (head-normal form)
-- e.g. (Pair[Int, b] : Eq) ~> (Int:Eq, b:Eq) ~> (b:Eq)

toHnf :: Pred -> TCM [Pred]
toHnf (t :~: u) = do
  subst1 <- mgu t u
  extSubst subst1
  return []

toHnf pred
  | inHnf pred = return [pred]
  | otherwise = do
      info ["> toHnf ", str pred]
      ce <- gets tcsIT
      case byInstM ce pred of
        Nothing -> throwError ("no instance of " ++ str pred)
        Just (preds, subst') -> do
            info ["! toHnf <  byInstM ", str preds, " subst'=", str subst']
            extSubst subst'
            toHnfs preds

toHnfs :: [Pred] -> TCM [Pred]
toHnfs ps = do
  subst <- getCurrentSubst
  info ["> toHnfs ", str ps, " subst=",str subst]
  ps2 <- simplifyEqualities ps >>= withCurrentSubst
  info ["< simpEqs ", str ps2]
  info  ["! toHnfs > toHnfs' ", str ps2]
  toHnfs' ps2

toHnfs' [] = return []
toHnfs' preds@(p:ps) = do
  info ["> toHnfs' ", str preds]
  rs1 <- toHnf p
  ps' <- withCurrentSubst ps           -- important
  rs2 <- toHnfs' ps'
  info ["< toHnfs' ", str (rs1++rs2)]
  return (rs1 ++ rs2)


inHnf :: Pred -> Bool
inHnf (InCls c args t) = hnf t where
  hnf (TVar _) = True
  hnf (TCon _ _) = False
inHnf (_ :~: _) = False


reduceContext :: [Pred] -> TCM [Pred]
reduceContext preds = do
  info ["> reduceContext ", str preds]
  ps2 <- (toHnfs preds >>= withCurrentSubst)
  info ["< toHnfs ", str ps2]
  return (nub ps2)

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
  let  subst = tcsSubst state
  let history = reverse (tcsLog state)
  putStrLn "History:"
  mapM_ putStrLn history
  putStrLn "------------------------------------------------------------------------"
  case result of
                  Left e -> do
                    putStrLn "Error: "
                    putStrLn e
                  Right t -> do
                    putStrLn expS
                    let typ = apply subst t
                    putStr ":: "
                    print typ

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

instance HasTypes Arg where
    apply s (UArg n) = UArg n
    apply s (TArg n t) = TArg n (apply s t)
    ftv (UArg n) = []
    ftv (TArg n t) = ftv t

-- instance HasTypes Expr where
