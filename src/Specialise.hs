module Specialise where
import Control.Monad.Error.Class(throwError)
import Data.List(intercalate)
import TCM
import Checker
import ISyntax
import Constraints
import Types

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
    Just (exp, subst) -> do
              fscheme <- askType n
              let (Forall _ (ps :=> typ)) = fscheme
              let tvs = ftv typ
              warn ["! specVar ", n, ":", str fscheme," @ ", str etyp,  " resolution: ", str (exp, subst)]
              phi <- mgu typ etyp `wrapError` ("specialise "++n, typ, etyp)
              let subst' = subst <> phi
              let tvs' = apply subst' (map TVar tvs)
              warn ["- specVar: tvs=", str tvs, " tvs'=", str tvs']
              let name' = specName n tvs'
              body' <- specialiseExp exp etyp
              warn ["< specExp ", str exp, " : ", str etyp, " ~>", str body']
              let args' = [] -- FIXME
              -- for noninlining version, add specialisation and return its name
              -- addSpecialisation name' etyp args' body'
              -- for inlining version, just return the specialised body
              addSpecialisation name' etyp args' body'
              return (EVar name')
    Nothing -> return e <* warn ["! specVar ", n, " to ", str etyp, " - NO res"]

specialiseExp e@(ECon n) etyp = specialiseCon n etyp

specialiseExp e@(EApp fun a) etyp = do
  (aps, atyp) <- tiExpr a `wrapError` a `wrapError` e
  (fps, ftyp) <- tiExpr fun `wrapError` fun `wrapError` e
  warn ["> specApp (", str e,
        ") fun = ", str fun," : ", str ftyp,
        "; arg = ", str a, " : ", str atyp,
        "; target: ", str etyp
       ]
  phi <- mgu ftyp (atyp :-> etyp) `wrapError` ("specialise", e)
  warn ["< mgu", str(ftyp, atyp :-> etyp), " = " , str phi]
  let atyp' = apply phi atyp
  let ftyp' = apply phi ftyp
  a' <- specialiseExp (apply phi a) atyp'
  f' <- specialiseExp (apply phi fun) ftyp'
  warn ["< specApp - fun = ",str (ETyped f' ftyp')," arg: ", str (ETyped a' atyp')]
  return (EApp f' a')

specialiseExp e@(ELam args body) etyp = withLocalEnv do
  let args' = attachTypes args (argTypes etyp)
  addArgs args'
  body' <- specialiseExp body (resultType etyp args)
  warn ["< specLam ", str e, " : ", str etyp, " ~>", str (ELam args' body')]
  return (ELam args' body')

specialiseExp e@(ETyped e' t) etyp = do
  e'' <- specialiseExp e' etyp
  return (ETyped e'' etyp)

specialiseExp e etyp = throwError("FAILED to specialise "++str e)


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
