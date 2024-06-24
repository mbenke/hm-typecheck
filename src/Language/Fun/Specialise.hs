module Language.Fun.Specialise where
import Control.Monad.Error.Class(throwError)
import Data.List(intercalate)
import TCM
-- import Language.Fun.Checker
import Language.Fun.ISyntax
import Language.Fun.Typecheck
import Language.Fun.Constraints
import Language.Fun.Types

specialiseEntry :: Name -> TCM ()
specialiseEntry name = do
  mdef <- lookupTLD name
  def@(name, scheme, args, body) <- maybeToTCM  ("No definition of " ++ name) mdef
  typ <- maybeToTCM ("Type of entry "++name++":"++ str scheme ++" is not monomorphic")
         (typeOfScheme scheme)
  warn ["! specialiseEntry ", name, " : ", str typ]
  body' <- specialiseBody args body typ
  info ["! new body: ", str body']
  let newDef = ValBind name args body'
  info ["! new def: ", str newDef]
  addSpecialisation name typ args body'
  return ()

specialiseBody :: [Arg] -> TcExpr -> Type -> TCM TcExpr
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

specialiseExp :: TcExpr -> Type -> TCM TcExpr
specialiseExp e@(EIntX _ i) etyp = pure e
specialiseExp e@(EVarX typ n) etyp = do
  mres <- lookupResolution n etyp
  case mres of
    Just (exp, dtyp, subst) -> do
              let tvs = ftv dtyp
              -- warn ["! specVar ", n, ":", str typ," @ ", str etyp,  " resolution: ", str (exp, subst)]
              phi <- mgu typ etyp `wrapError` ("specialise "++n, typ, etyp)
              let subst' = subst <> phi
              let tvs' = apply subst' (map TVar tvs)
              -- warn ["- specVar: tvs=", str tvs, " tvs'=", str tvs']
              let name' = specName n tvs'
              body' <- specialiseExp exp etyp
              warn ["< specExp ", str exp, " : ", str etyp, " ~>", str body']
              let args' = [] -- FIXME
              -- for noninlining version, add specialisation and return its name
              -- addSpecialisation name' etyp args' body'
              -- for inlining version, just return the specialised body
              addSpecialisation name' etyp args' body'
              return (EVarX etyp name')
    Nothing -> return e <* warn ["! specVar ", n, " to ", str etyp, " - NO res"]
specialiseExp e@(EConX contyp n) etyp = specialiseCon n contyp etyp
specialiseExp e@(EAppX _ fun a) etyp = do
  let atyp = typeOfTcExpr a
  let ftyp = typeOfTcExpr fun
  warn [ "> specApp (", str e
       , ") fun = ", str fun," : ", str ftyp
       , "; arg = ", str a, " : ", str atyp
       , "; target: ", str etyp
       ]
  phi <- mgu ftyp (atyp :-> etyp) `wrapError` ("specialise", e)
  -- warn ["< mgu", str (ftyp, atyp :-> etyp), " = " , str phi]
  let atyp' = apply phi atyp
  let ftyp' = apply phi ftyp
  a' <- specialiseExp (apply phi a) atyp'
  f' <- specialiseExp (apply phi fun) ftyp'
  warn ["< specApp - fun = ",str (ETypedX mempty f' ftyp')," arg: ", str (ETypedX mempty a' atyp')]
  return (EAppX  mempty f' a')

specialiseExp e@(ELamX x args body) etyp = do
  -- let args' = attachTypes args (argTypes etyp)
  warn ["> specArgs ", str args, " : ", str etyp]
  (args', rt, subst) <- specArgs args etyp
  warn ["< specArgs ~> ", str subst]
  body' <- specialiseExp (apply subst body) (resultType etyp args)
  warn ["< specLam ", str e, " : ", str etyp, " ~>", str (ELamX x args body')]
  return (ELamX x args' body')
  where
    specArgs [] t = pure ([], t, mempty)
    specArgs args@(TArg name typ:as) (t :-> u) = do
      subst1 <- mgu typ t `wrapError` ("specArgs "++name, args, t :-> u)
      (as', rt, subst2) <- specArgs (apply subst1 as) (apply subst1 u)
      return (TArg name (apply subst1 typ):as', rt, subst1 <> subst2)
    specArgs _ t = error("specialiseExp: "++str t++" is not a function type")

specialiseExp e@(ETypedX x e' t) etyp = do
  e'' <- specialiseExp e' etyp
  return (ETypedX x e'' etyp)

specialiseExp (EBlockX x stmts) etyp = withLocalEnv do
  stmts' <- mapM (`specialiseStmt` etyp) stmts
  return (EBlockX x stmts')

specialiseExp (ECaseX rtyp e alts) etyp = do
  let styp = typeOfTcExpr e
  -- NOTE: typechecking alts may impose constraints on scrutinee e
  alts' <- mapM (\alt -> specialiseAlt alt styp etyp) alts
  return (ECaseX etyp (ETypedX mempty e styp) alts')
-- this should never happen, but just in case:
specialiseExp e etyp = throwError ("FAILED to specialise "++str e)

specialiseAlt :: TcCaseAlt -> Type -> Type -> TCM TcCaseAlt
specialiseAlt (CaseAltX x con as e) styp etyp = withLocalEnv do
  -- styp - scrutinee type; etyp - expected result type
  warn ["> specAlt ", "(",con, str as," : ",str styp,")"," -> ", str e,":", str etyp]
  conscheme <- askType con  -- FIXME: decorate case alts instead
  let (Forall _ (_ :=> contyp)) = conscheme
  phi <- mgu (resultType contyp as) styp `wrapError` ("specialiseAlt "++con, contyp, styp)
  let contyp' = apply phi contyp
  let ats = argTypes contyp'
  let as' = attachTypes as ats
  addArgs as'
  e' <- specialiseExp (apply phi e) etyp
  warn ["< specAlt ", str (CaseAltX  x con as' e')]
  return (CaseAltX x con as' e')

specialiseCon :: Name -> Type -> Type -> TCM TcExpr
specialiseCon name contyp etyp = do
  let tvs = ftv contyp
  phi <- mgu contyp etyp
  let tvs' = apply phi (map TVar tvs)
  -- let name' = specName name tvs'
  let name' = name -- FIXME: need better specialisation for data types
  warn ["! specCon ",name," : ", str contyp, " to ", name', " : ", str etyp]
  return (EConX etyp name')

specName :: Name -> [Type] -> Name
specName n [] = n
specName n ts = n ++ "$" ++ intercalate "_" (map str ts)
-- specName n ts = n ++ "<" ++ intercalate "," (map str ts) ++ ">"
-- alternatively we could use similar Unicode letter characters:
-- Canadian Syllabics Pa (U+1438), Po (U+1433), and Final Short Horizontal Stroke (U+1428)
-- specName n ts = n ++ "\x1438" ++ intercalate "\x1428" (map str ts) ++ "\x1433"
-- or mangle, e.g. using $ and underscore:
-- specName n ts = n ++ "$" ++ intercalate "_" (map str ts) ++ "$"

specialiseStmt :: TcStmt -> Type -> TCM TcStmt
specialiseStmt (SExprX a e) _ = do
  let t = typeOfTcExpr e
  e' <- specialiseExp e t
  return (SExprX a e')
specialiseStmt (SAlloc a x t) _ = do
    extEnv x (monotype $ stackT t)
    return (SAlloc a x t)
