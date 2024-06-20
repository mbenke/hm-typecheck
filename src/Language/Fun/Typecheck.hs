{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}  -- for a general ToStr instance
module Language.Fun.Typecheck where
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List((\\), union, nub, intercalate)
import qualified Data.Map as Map

import Language.Fun.ISyntax
import Language.Fun.Types
import Language.Fun.Constraints
    ( HasTypes(apply, ftv), Subst, expel, match, mgu, matchPred )
import Common.NameSupply
import TCM
import Common.Debug
import Language.Fun.Phase

type instance XLam FunTc = NoExtField
type instance XLet FunTc = NoExtField
type instance XApp FunTc = NoExtField
type instance XVapp FunTc = NoExtField
type instance XVar FunTc = Type
type instance XCon FunTc = Type
type instance XInt FunTc = NoExtField
type instance XBlock FunTc = NoExtField
type instance XTyped FunTc = NoExtField
type instance XCase FunTc = NoExtField
type instance XExp FunTc = DataConCantHappen

type TcExpr = ExpX FunTc
pattern TcInt :: Integer -> TcExpr
pattern TcInt n = EIntX NoExtField n
pattern TcApp :: TcExpr -> TcExpr -> TcExpr
pattern TcApp e1 e2 = EAppX NoExtField e1 e2

type TcDecl = DeclX FunTc
type TcProg = ProgX FunTc
type TcBind = BindX FunTc

instance HasTypes TcExpr where
  apply s (ELamX _ args e) = ELamX NoExtField args (apply s e)
  apply s (ELetX _ x e1 e2) = ELetX NoExtField x (apply s e1) (apply s e2)
  apply s (EAppX _ e1 e2) = EAppX NoExtField (apply s e1) (apply s e2)
  apply s (EVappX _ e es) = EVappX NoExtField (apply s e) (map (apply s) es)
  apply s (EVarX t n) = EVarX (apply s t) n
  apply s (EConX t n) = EConX (apply s t) n
  apply s (EIntX _ n) = EIntX NoExtField n
  apply s (EBlockX _ stmts) = EBlockX NoExtField stmts
  apply s (ETypedX _ e t) = ETypedX NoExtField (apply s e) t
  apply s (ECaseX _ e alts) = ECaseX NoExtField (apply s e) alts
  apply s (ExpX a) = absurd a
  ftv (ELamX _ args e) = ftv e
  ftv (ELetX _ _ e1 e2) = ftv e1 `union` ftv e2
  ftv (EAppX _ e1 e2) = ftv e1 `union` ftv e2
  ftv (EVappX _ e es) = foldr union [] (map ftv (e:es))
  ftv (EVarX t _) = ftv t
  ftv (EConX t _) = ftv t
  ftv (EIntX _ _) = []
  ftv (EBlockX _ stmts) = []
  ftv (ETypedX _ e _) = ftv e
  ftv (ECaseX _ e alts) = ftv e -- `union` foldr union [] (map ftv alts)

instance Show TcExpr where
  showsPrec d (EIntX _ n) = showsPrec 10 n
  showsPrec d (EVarX t n) = showParen True $
             showString n .showString ": " . shows t
  showsPrec d (EConX t n) = showParen True $
             showString n .showString ": " . shows t
  showsPrec d (EAppX _ e1 e2) = showParen (d > ap_prec) $
             showsPrec ap_prec e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
         where ap_prec = 10

  showsPrec d (ELamX _ args e) = showParen (d > lam_prec) $
             showString "\\" . showArgs args . showString " -> " .
             showsPrec lam_prec e
         where
           lam_prec = 1
           showArgs = showString . unwords  . map showArg

  showsPrec d (ELetX _ x e1 e2) = showParen (d > let_prec) $
             showString "let " . showString x . showString "= " . showsPrec 0 e1 .
             showsPrec let_prec e2
         where let_prec = 2

  showsPrec d (EBlockX _ stmts) = showString "{\n  " .
                               showString ( intercalate ";\n  " (map show stmts)) .
                               showString "\n}"
  showsPrec d (ETypedX _ e t) = showParen (d > typ_prec) $
             shows e .
             showString " : " . shows t
         where typ_prec = 2
  showsPrec d (ECaseX _ e alts) = showString "case " . showsPrec 0 e . showString " of {\n  " .
                               showString ( intercalate ";\n  " (map show alts)) .
                               showString "\n}"

showArg :: Arg -> String
showArg (UArg s) = s
showArg (TArg s t) = concat ["(",s,":",show t,")"]

instance Show TcDecl where
  show :: TcDecl -> String
  show (TypeDecl t alts) = unwords [show t, "=", salts]
     where salts = intercalate " | " (map show alts)
  show (ValDecl n qt) = unwords [n, ":", show qt]
  show (ValBind n [] e) = unwords [n, "=", show e]
  show (ValBind n as e) = unwords [n, sas, "=", show e] where
    sas = unwords (map showArg as)
  show (ClsDecl pred mdecls) = unwords ["class", show pred, "{",  showDecls mdecls, "}"] where
    showDecls ds = intercalate "; " (map showDecl ds)
  show (InstDecl pred mdecls) = unwords ["instance", show pred]
  show (Mutual ds) = "mutual {" ++ intercalate ";\n " (map show ds) ++ "}"
  show (Pragma prag) = "pragma " ++ prag

tiExpr :: Expr -> TCM ([Pred], Type) -- FIXME: transitional
tiExpr e = do
  (_, ps, t) <- tcExpr e
  pure (ps,t)

instance Show TcProg where
  show (Prog  decls) = intercalate ";\n" (map show decls)

tcExpr :: Expr -> TCM (TcExpr, [Pred], Type)
tcExpr e@(EInt n) = pure (TcInt n, [], TInt)
-- check a lambda (function) such as `\x y -> x`
tcExpr e@(ELam args e1) = do
  env <- getEnv
  argTypes <- addArgs args
  (tce1, ps, t1) <- tcExpr e1
  -- typing function body may have created constraints on argument types
  -- these are recorded in the current substitution, which needs to be applied here
  argTypes' <- withCurrentSubst argTypes
  tce1' <- withCurrentSubst tce1
  putEnv env
  let typedArgs = zipWith addType args argTypes'
  pure (ELamX mempty typedArgs tce1, ps, foldr (:->) t1 argTypes')
  where
    addType :: Arg -> Type -> Arg
    addType (UArg name) t = TArg name t
    addType arg _ = arg

tcExpr (EVar v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (EVarX t v, ps, t)

tcExpr (ECon v) = do
  s <- askType v
  ps :=> t <- freshInst s
  pure (EConX t v, ps, t)

-- check an application (function call)
tcExpr e@(EApp fun arg) = do
  (tcfun, ps, t1) <- tcExpr fun
  (tcarg, qs, t2) <- tcExpr arg
  freshname <- tcmDeplete
  let tr = TVar freshname
  unify t1 (t2 :-> tr) `wrapError` e
  s <- getSubst
  let tr' = apply s tr
  let preds = apply s (ps ++ qs)
  let tcfun' = apply s tcfun
  let tcarg' = apply s tcarg
  -- info ["< EApp ", str fun, ":", str t1, "@", str arg,  " : ", str (preds :=> tr')]
  return (EAppX mempty tcfun' tcarg', preds, tr')

tcExpr exp@(ELet x e1 e2) = withLocalEnv do
  (tce1, ps, t1) <- tcExpr e1
  tcBindgroup [ValBind x [] e1]
  (tce2, qs, t) <- tcExpr e2
  return (ELetX mempty x tce1 tce2, qs, t) -- FIXME

tcExpr (ETyped e t) = do
  (tce, ps, t1) <- tcExpr e
  unify t t1
  tce1 <- withCurrentSubst tce
  return (tce1, ps, t1)

tcExpr e@(EBlock stmts) = do --FIXME: TcStmts
  withLocalEnv (go stmts) 
  return (EBlockX mempty stmts, [], unitT) where 
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
      Forall as (ps1 :=> t1) <- generalize (ps, t) `wrapError` e
      info [showSmallEnv localEnv, " |- ", str e, " : ", str $ ps1 :=> t1]
      return unit

    tiStmt (SAlloc ann n t) = do
      extEnv n (monotype $ stackT t)
      q <- askType n
      info ["alloc ", n, " : ", str t, " ~> ", n, " : ", str q]
      return unit

tcExpr (ECase scrut alts) = do -- TODO unify scrutinised expr type with con target
  targetType <- TVar <$> tcmDeplete
  (tcscrut, ps, scrutType) <- tcExpr scrut   -- scrutinisied expresion
  (tcalts, ps') <- tcAlts scrutType targetType alts
  resultType <- withCurrentSubst targetType
  return (ECaseX mempty tcscrut tcalts, ps ++ ps', resultType)

tcAlts :: Type -> Type -> [CaseAlt] -> TCM ([CaseAlt], [Pred])
tcAlts scrut target alts = do
  ps <- tiAlts scrut target alts
  return (alts, ps)

tiAlts ::Type -> Type -> [CaseAlt] -> TCM [Pred]
tiAlts scrut target [] = return []
tiAlts scrut target (alt:alts) = do
  warn ["> tiAlts, target : ", str target]
  (ps, expected) <- tiAlt target alt
  unify scrut expected `wrapError` alt
  target' <- withCurrentSubst target
  scrut' <- withCurrentSubst scrut
  ps' <- tiAlts scrut' target' alts
  return (ps ++ ps')


tiAlt :: Type -> CaseAlt -> TCM ([Pred], Type)  -- result to be unified with scrutType
tiAlt target alt@(CaseAlt con args e) = withLocalEnv do
  warn ["> tiAlt ", str alt, " : ", str target]
  conScheme <- askType con
  (qs :=> conType) <- freshInst conScheme
  (argTypes, conResultType) <- addConArgs conType args
  warn ["! tiAlt ", con, ":", str conType, " ", str argTypes]
  (ps,t) <- tiExpr e
  unify target t `wrapError` alt
  withCurrentSubst (ps, conResultType)

addConArgs :: Type -> [Arg] -> TCM ([Type], Type)
addConArgs conType [] = return ([], conType)
addConArgs (at :-> rest) (arg:args) = do
  let name = argName arg
  extEnv name (monotype at)
  (ats, rt) <- addConArgs rest args
  return (at:ats, rt)

------------------------------------------------------------

tiBind :: Bind -> TCM ([Pred], Type)
tiBind b = snd <$> tcBind b

tcBind :: Bind -> TCM (TcBind, ([Pred], Type))
tcBind (Bind n args e) = withLocalEnv do
  as <- addArgs args
  (tce0, ps0, t0) <- tcExpr e
  let t1 = foldr (:->) t0 as
  -- Unify the inferred type with the assumed type variable
  assumed <- askType n
  (aq :=> at) <- freshInst assumed
  unify at t1
  (tce, ps :=> t) <- withCurrentSubst (tce0, ps0 :=> t1)
  warn ["< tiBind ", n, " : ", str (ps :=> t)]
  return (Bind n args tce, (ps, t))


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
  envVars <- getFreeEnvVars
  (ps1, t1) <- withCurrentSubst (ps0, t0)
  ce <- gets tcsCT
  ps2 <- reduceContext ps1
  -- reduceContext may have extended the subst, need to reapply it
  t2 <- withCurrentSubst t1
  let typeVars =  ftv (ps2, t2)
  let scheme = Forall (typeVars \\ envVars) (ps2 :=> t2)
  info ["< generalize: ", str (legibleScheme scheme)]
  return scheme

schemeOf :: Expr -> TCM Scheme
schemeOf exp = wrapError ty exp where
  ty = tiExpr exp >>= generalize

tiDecl decl = tcDecl decl >> return ()

tcDecl :: Decl -> TCM TcDecl
tcDecl (ValDecl n qt) = do
  let tvs = ftv qt
  let s = Forall tvs qt
  extEnv n s
  return (ValDecl n qt) 

tcDecl d@(ValBind n as e) = do
  (Bind n' as' e') <- tcBindgroup1 d
  Forall tvs (qs :=> typ) <- askType n
  let exp = formLambda as typ e
  addResolution n typ exp
  return (ValBind n as e')
tcDecl (Mutual ds) = do
  tcbs <- tcBindgroup ds 
  return $  Mutual [ValBind n a e | Bind n a e <- tcbs ]

-- check type declaration,such as `Option a = None |  Some a`
tcDecl decl@(TypeDecl typ@(TCon name args) alts) = do
  constructors <- tiConAlts typ alts
  forM_ constructors addCon
  let consNames = map fst constructors
  let arity = length args
  let typeInfo = (arity, consNames)
  modify (addTypeInfo name typeInfo)
  return (TypeDecl typ alts)
  where
      addCon (name, typ) = extEnv name typ

-- check instance declaration such as `instance Int : Eq`
tcDecl (InstDecl qp methods) = do
  tcms <- tcInstance qp methods
  return (InstDecl qp tcms)

-- check class declaration such as `class a:Eq { eq : a -> a -> Bool }`
tcDecl decl@(ClsDecl pred@(InCls c as (TVar mainTVar)) methods) = do
  tcMethods <- forM methods tcMD
  let (methodNames, tcMethodDecls) = unzip tcMethods
  let classInfo = (className, methodNames)
  modify (addClassInfo className (classArity, methodNames)) 
  return (ClsDecl pred tcMethodDecls)
  where
    className = predName pred
    classArity = length $ predArgs pred
    tcMD (ValDecl name ([] :=> typ)) = do
      -- ambiguity check
      unless (mainTVar `elem` ftv typ) $ throwError $ unlines
        [ "- in the declaration of class " ++ className ++ ":"
        , "  ambiguous type variable in method " ++ name ++ ": " ++ show typ
        ]
      extEnv name $ Forall (ftv typ) ([pred] :=> typ)
      return (name, ValDecl name ([] :=> typ))
    tcMD (ValDecl name (preds :=> typ)) = throwError $ unlines
           [ "- in the declaration of class" ++ predName pred
           , "- in method " ++ name ++ ":"
           , "  qualifiers not allowed in method types"
           ]

    tcMD decl = throwError $ unlines
           [ "- in the declaration of class " ++ className ++ ":"
           , "  only type declarations allowed, illegal declaration: " ++ show decl
           ]
tcDecl(ClsDecl (InCls c as complexType) _) = throwError $ unlines
           [ "- in the declaration of class " ++ c ++ ":"
           , "  main argument must be a type variable, illegal type: " ++ show complexType
           ]
tcDecl decl@(Pragma prag) = process prag >> return (Pragma prag) where
    process "log" = void $ setLogging True >> warn ["-- Logging ON  --"]
    process "nolog" = setLogging False >> warn ["-- Logging OFF --"]
    process "nocoverage" = setCoverageCheck False >> warn ["-- Coverage Checking OFF --"]
    process "coverage" = setCoverageCheck True >> warn ["-- Coverage Checking OFF --"]

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
tiInstance qp d = tcInstance qp d >> return ()

tcInstance :: Qual Pred -> [Decl] -> TCM [TcDecl]
tcInstance inst@(constraint :=> ihead@(InCls c as t)) methods = do
  let 
    header :: Decl = InstDecl inst [] -- for error messages
  -- warn ["+ tiInstance ", str inst]
  ois <- getInsts c `wrapError` header
  checkOverlap t ois
  enabled <- gets tcsCoverageEnabled
  when enabled $ checkCoverage c as t `wrapError` header
  checkMeasure constraint ihead `wrapError` header
  tcbs <- forM methods (checkMethod ihead)
  let anf = anfInstance inst
  modify (addInstInfo anf)
  return [ValBind n a e |  Bind n a e <- tcbs]
  where
    checkMeasure :: [Pred] -> Pred -> TCM ()
    checkMeasure constraint ihead =
      if measure constraint < measure ihead then return ()
      else throwError $ unwords ["size of constraints must smaller than the head"]

    checkOverlap :: Type -> [Inst] -> TCM ()
    checkOverlap t [] = pure ()
    checkOverlap t (oi:is) = do
      -- Type variables in an instance declaration are implicitly bound
      -- so they need to be renamed before the overlap check
      -- doing this here rather than in tiInstance for better error msgs
      oi' <- renameFreeVars oi
      case oi' of
        (_ :=> InCls _ _ u) -> case mgu t u of
          Right s -> throwError
                 (unwords ["instance",str inst,"overlaps", str oi])
          Left _ -> checkOverlap t is
        (_ :=> (t1 :~: t2)) ->
          error(unwords["internal error: illegal instance", str oi])


    checkCoverage :: String -> [Type] -> Type -> TCM ()
    checkCoverage c as t = do
      let strongTVs = ftv t
      let weakTVs = ftv as
      let undetermined = weakTVs \\ strongTVs
      unless (null undetermined) $ do
        let undetermined_str = intercalate ", " undetermined
        throwError $ unwords
          [ "Coverage condition fails in class", c
          , "- the type", str t, "does not determine"
          , undetermined_str
          ]

    findPred :: Name -> [Pred] -> Maybe Pred
    findPred cname (p:ps) | predName p == cname = Just p
                          | otherwise = findPred cname ps
    findPred cname [] = Nothing
    checkMethod :: Pred -> Decl-> TCM TcBind
    checkMethod ihead (ValBind name args body) = do
      let InCls cname cargs mainType = ihead
      Forall tvs (qs :=> genType)  <- askType name
      p <- maybeToTCM ("Constraint for "++cname++ " not found in type of "++name)
                      (findPred cname qs)
      subst <- liftEither (matchPred p ihead) `wrapError` ihead
      let expType = apply subst genType
      let args' = apply subst args
      let exp = formLambda args' expType body
      let iTypes = apply subst (map TVar tvs)
      (tce, iq, it) <- tcExpr exp
      warn ["< tiExpr ", str exp, " : ", str (iq:=>it)]
      match it expType `wrapError` exp
      addResolution name expType exp
      return (Bind name args' tce)

formLambda :: [Arg] -> Type -> Expr -> Expr
formLambda [] typ body = body
formLambda as typ body = ELam as body where
  addTypes :: [Arg] -> Type -> [Arg]
  addTypes [] t = []
  addTypes (a:as) (t :-> u)  = TArg (argName a) t : addTypes as u
  addTypes _ _ = error "formLambda: wrong number of arguments"

tiProg p = tcProg p >> return ()

tcProg :: Prog -> TCM TcProg
tcProg (Prog decls) = Prog <$> mapM tcDecl decls

-- check a bindgroup containing one bind
tcBindgroup1 :: Decl -> TCM TcBind
tcBindgroup1 (ValBind n as e) = do
  tcbs <- tcBindgroup [ValBind n as e]
  case tcbs of
    [b] -> return b
    _ -> throwError "internal error: tcBindgroup1"

tcBindgroup :: [Decl] -> TCM [TcBind]   -- TODO: specialisation
tcBindgroup binds = do
  binds <- scanDecls binds
  -- warn ["> tiBindgroup ", str binds]
  -- cannot clean substitution when doing mutual recursion
  qtBinds <- mapM tcBind binds
  let (tcbs, qts) = unzip qtBinds
  qts' <- withCurrentSubst qts
  schemes <- mapM generalize qts'
  let names = map bindName binds
  let results = zip names schemes
  mapM_ (uncurry extEnv) results
  return [Bind n a e | Bind n a e <- tcbs]
  where
    scanDecls :: [Decl] -> TCM [Bind]
    scanDecls [] = return []
    scanDecls (ValDecl n qt:ValBind n' as e:ds) | n == n' = do
    -- explicitly typed binding, add declared type to the environment
      let tvs = ftv qt
      let s = Forall tvs qt
      extEnv n s
      bs <- scanDecls ds
      return (Bind n as e:bs)
    scanDecls (ValBind n as e:ds) = do
    -- untyped binding, add fresh type variable to the environment
      a <- TVar <$> tcmDeplete
      extEnv n (monotype a)
      bs <- scanDecls ds
      return (Bind n as e:bs)
    scanDecls (d:ds) = throwError ("illegal declaration in mutual: " ++ show d)

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

toHnf :: Int -> Pred -> TCM [Pred]
toHnf _ (t :~: u) = do
  subst1 <- mgu t u
  extSubst subst1
  return []

toHnf depth pred
  | inHnf pred = return [pred]
  | otherwise = do
      ce <- gets tcsIT
      case byInstM ce pred of
        Nothing -> throwError ("no instance of " ++ str pred
                  ++"\nKnown instances:\n"++str ce)
        Just (preds, subst') -> do
            extSubst subst'
            toHnfs (depth-1) preds

toHnfs :: Int -> [Pred] -> TCM [Pred]
toHnfs depth ps = do
  subst <- getCurrentSubst
  ps2 <- simplifyEqualities ps >>= withCurrentSubst
  toHnfs' depth ps2

toHnfs' _ [] = return []
toHnfs' 0 ps = throwError("Max context reduction depth exceeded")
toHnfs' d preds@(p:ps) = do
  let d' = d-1
  rs1 <- toHnf d' p
  ps' <- withCurrentSubst ps   -- important, toHnf may have extended the subst
  rs2 <- toHnfs' d' ps'
  return (rs1 ++ rs2)


inHnf :: Pred -> Bool
inHnf (InCls c args t) = hnf t where
  hnf (TVar _) = True
  hnf (TCon _ _) = False
inHnf (_ :~: _) = False


reduceContext :: [Pred] -> TCM [Pred]
reduceContext preds = do
  let depth = maxReductionDepth
  unless (null preds) $ info ["> reduceContext ", str preds]
  ps1 <- toHnfs depth preds `wrapError` preds
  ps2 <- withCurrentSubst ps1
  unless (null preds) $ info ["< reduceContext ", str ps2]
  return (nub ps2)

maxReductionDepth :: Int
maxReductionDepth = 100
