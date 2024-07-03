module Language.Fun.EmitCore where
import Language.Core(Core(..))
import Language.Core qualified as Core
import TCM
import Data.Map qualified as Map
import Control.Monad(forM)
import Control.Monad.Reader.Class
import Control.Monad.State(gets)
import GHC.Stack
import Language.Fun.ISyntax(Name, ToStr(..), Expr(..), ExpX(..), typeOfTcExpr)
import Language.Fun.ISyntax qualified as Fun
import Language.Fun.Types qualified as Fun
import Language.Fun.Types(Qual((:=>)), pattern (:->))
import Language.Fun.Constraints(Subst(..), apply)

type VSubst = Map.Map Name Core.Expr
emptyVSubst :: VSubst
emptyVSubst = Map.empty

data  ECEnv = ECEnv { ecSubst :: VSubst, ecTT :: TypeTable}


extendECEnv :: VSubst -> ECEnv -> ECEnv
extendECEnv subst env = env { ecSubst = ecSubst env <> subst}

readConstructors :: Name -> ECEnv -> [ConInfo]
readConstructors name env = case Map.lookup name (ecTT env) of
    Just (arity, cs) -> cs
    _ -> error ("readConstructors: unknown type "++str name)

conSiblings :: Name -> ECEnv -> [ConInfo]
conSiblings con env = lookupConSiblings con (ecTT env)

emitCore :: TCM Core
emitCore = do
    specTable <- gets tcsSpec
    cores <- mapM emitSpecs (Map.toList specTable)
    pure (Core (concat cores))

emitSpecs :: (Name, [Specialisation]) -> TCM [Core.Stmt]
emitSpecs (name, specs) = mapM (emitSpec name) specs

-- | `emitSpec` emits core for a single specialised definition
emitSpec :: Name -> Specialisation -> TCM Core.Stmt
emitSpec origName (name, typ, args, body) = do
    let (lambody, lamas) = stripLambda body
    lambody' <- etaLong lambody
    let (body', extraArgs) = stripLambda lambody'
    let allArgs = args ++ lamas ++ extraArgs
    -- warn ["> emitSpec ", name, " : ", str typ]
    -- warn ["! body=", str body]
    -- warn ["! long body=", str body']
    -- warn ["! emitSpec: allArgs=", str allArgs]
    -- warn ["! emitSpec: lambody=", str lambody]
    typeTable <- getTypeTable
    let coreArgs = map (translateArg typeTable) allArgs
    let resultType = typeOfTcExpr body'
    let coreType = translateType typeTable resultType
    coreBody <- translateBody body'
    return (Core.SFunction name coreArgs coreType coreBody)
    -- return (Core.SFunction name coreArgs coreType [])


-- | `etaLong` builds an eta-long form of an expression
-- i.e. if an expression is of function type, it is converted
-- to a lambda with a fresh variable as an argument
etaLong :: Fun.TcExpr -> TCM Fun.TcExpr
etaLong (Fun.ELamX x args body) = do
    body' <- etaLong body
    return (Fun.ELamX x args body')
etaLong e = do
    (xs, body) <- go (typeOfTcExpr e) e
    case xs of
        [] -> return body
        _ -> return  (Fun.ELamX  mempty (mkArgs xs) (Fun.EVappX mempty e (mkVars xs)))
    where
        mkArgs :: [(Name, Fun.Type)] -> [Fun.Arg]
        mkArgs = map (uncurry Fun.TArg)
        mkVars :: [(Name, Fun.Type)] -> [Fun.TcExpr]
        mkVars = map (\(n, t) -> Fun.EVarX t n)
        go :: Fun.Type -> Fun.TcExpr -> TCM ([(Name, Fun.Type)], Fun.TcExpr)
        go (t :-> u) e = do
            s <- tcmDeplete
            let x = ('$':s, t)
            (xs, e') <- go u e
            return (x:xs, e')
        go t e = pure ([], e)

type ECReader a = ECEnv -> a
type Translation a = ECReader([Core.Stmt], a)

translateBody :: Fun.TcExpr -> TCM [Core.Stmt]
translateBody exp = do
    typeTable <- gets tcsTT
    let env = ECEnv emptyVSubst typeTable
    let (code, result) = translateExp exp env
    return (code ++ [Core.SReturn result])

translateExp :: Fun.TcExpr -> Translation Core.Expr
translateExp (Fun.TcInt n) = pure ([], Core.EInt (fromInteger n))
translateExp (Fun.EVarX t n) = do
    replace <- reader (Map.lookup n . ecSubst)
    case replace of
        Just e -> pure ([], e)
        Nothing -> pure ([], Core.EVar n)
translateExp e@(Fun.EAppX _ f a) = do
    let (g, as) = unwindApp e
    case g of
        Fun.EVarX _ f -> translateExp (Fun.EVappX mempty g as)
        Fun.EConX t c -> translateConApp c as
        _ -> error ("translateExp/EApp: not implemented for "++str e)
translateExp (Fun.EConX t c) = translateConApp c []
translateExp (Fun.EVappX _ (Fun.EVarX t f) as) = do
    targs <- mapM translateExp as
    let (codes, coreArgs) = unzip targs
    let call = Core.ECall f coreArgs
    let code = concat codes
    pure (code, call)

{-
General approach to case expression translation:
1. find scrutinee type
2. fetch constructor list
3. Check for catch-all case alt (opt)
4. Build alternative map, initially containing error or catch-all
5. Translate case alts and insert them into the alt map
6. Build nested match statement from the map

Start with special case for product types - just transform to projections
-}



translateExp (Fun.ECaseX rtyp p [alt]) = do
    (pcode, pval) <- translateExp p
    (bcode, bval) <- translateOnlyAlt pval alt
    pure (pcode ++ bcode, bval)

translateExp (Fun.ECaseX rtyp (ETypedX _ p styp) alts) = do
    let styp = typeOfTcExpr p
    typeTable <- reader ecTT
    let coreResultType = translateType typeTable rtyp
    let allocResult = [Core.SAlloc "_caseresult" coreResultType]
    (pcode, pval) <- translateExp p
    let typename = case styp of
            Fun.TCon name _ -> name
            _ -> error ("translateExp: case with non-constructor type "++str styp)
    constructors <- readConstructors typename
    let conNames = map fst constructors
    let noMatch c = [Core.SRevert ("no match for: "++c)]
    let defaultAltMap = Map.fromList [(c, noMatch c) | c <- conNames]
    branches <- transBranches alts
    let altMap = foldr (\(c, bcode, bval) m -> Map.insert c bcode m) defaultAltMap branches
    let casecode = buildMatch pval altMap constructors
    pure (pcode ++ allocResult ++ casecode, Core.EVar "_caseresult")
    where
        transBranch :: Core.Expr -> Fun.TcCaseAlt -> ECReader (Name, [Core.Stmt], Core.Expr)
        transBranch pval alt@(Fun.CaseAltX _ con args body) = do
            (bcode, bval) <- translateAltInto "_caseresult" pval alt
            pure (con, bcode, bval)
        transBranches :: [Fun.TcCaseAlt] -> ECReader [(Name, [Core.Stmt], Core.Expr)]
        transBranches [alt] = (:[]) <$> transBranch (Core.EVar "right") alt
        transBranches (alt:alts) = do
            b <- transBranch (Core.EVar "left") alt
            bs <- transBranches alts
            pure (b:bs)
translateExp (Fun.ETypedX _ e t) = translateExp e
-- translateExp (Fun.ECaseX rtyp p [alt]) = error "translateExp: case with untyped scrutinee"
translateExp exp = error ("translateExp: not implemented for "++str exp)

buildMatch :: Core.Expr -> Map.Map Name [Core.Stmt] -> [ConInfo] -> [Core.Stmt]
buildMatch pval altMap cons = -- error("buildMatch: not implemented" ++ show altMap)
  go cons where
    go :: [ConInfo] -> [Core.Stmt]
    go [(name, s)] = altMap Map.! name
    go ((name, s):cons) =
        [Core.SMatch pval [ alt "left" (altMap Map.! name)
                          , alt "right" (go cons)]]
    alt n [stmt] = Core.Alt n stmt
    alt n stmts = Core.Alt n (Core.SBlock stmts)

translateAltInto :: Name -> Core.Expr -> Fun.TcCaseAlt -> Translation Core.Expr
translateAltInto name pval (Fun.CaseAltX _ con args body) = do
    let pvars = translatePatArgs pval args
    let caseresult = Core.EVar name
    (code, val) <- local (extendECEnv pvars) $ translateExp body
    let assign = Core.SAssign caseresult val
    pure (code ++ [assign], caseresult)

-- if there's only one case alt (unpacking a product type), we can skip caseresult
translateOnlyAlt :: Core.Expr -> Fun.TcCaseAlt -> Translation Core.Expr
translateOnlyAlt pval (Fun.CaseAltX _ con args body) = do
    let pvars = translatePatArgs pval args
    local (extendECEnv pvars) $ translateExp body

unwindApp :: Fun.TcExpr -> (Fun.TcExpr, [Fun.TcExpr])
unwindApp e = go e [] where
    go (Fun.EAppX _ f a) as = go f (a:as)
    go f as = (f, as)

stripLambda :: Fun.TcExpr -> (Fun.TcExpr, [Fun.Arg])
stripLambda (Fun.ELamX x args body) = let (b, as) = stripLambda body in (b, args ++ as)
stripLambda e = (e, [])

translateArg :: TypeTable -> Fun.Arg -> Core.Arg
translateArg tt (Fun.TArg n t) = Core.TArg n (translateType tt t)
translateArg _ (Fun.UArg n) = error("translateArg: UArg "++n)

translateType :: HasCallStack => TypeTable -> Fun.Type -> Core.Type
translateType _ Fun.TInt = Core.TInt
translateType _ Fun.TBool = Core.TBool
translateType _ Fun.TUnit = Core.TUnit
translateType _ t@(u :-> v) = error ("Cannot translate function type " ++ str t)
translateType tt (Fun.TCon name tas) = translateTCon tt name tas

-- translate (monomorphic) datatypes to sums of products
-- constructors are translated to inr/inl chains
-- e.g. data Color = Red | Green | Blue
-- Red -> 0 inr/ 1 inl  | G -> 1/1 | B -> 2/0
-- NB no inr/inl with one constructor, e.g.
-- type Pair a b = Pair a b; Pair -> 0/0

translateTCon :: TypeTable -> Name -> [Fun.Type] -> Core.Type
translateTCon tt tycon tas =
      buildSumType (map (translateDCon tt tas) cons)
    where
      cons = case Map.lookup tycon tt of
        Just (arity, cs) -> cs
        _ -> error ("translateTCon: unknown type "++str tycon)
      buildSumType [] = Core.TUnit
      buildSumType ts = foldr1 Core.TSum ts


translateDCon :: TypeTable -> [Fun.Type] -> ConInfo -> Core.Type
-- translateDCon tt targs (name, Fun.Forall [] ([] Fun.:=> typ)) =
-- translateProductType tt tas where (tas, tr) = Fun.unwindType typ

translateDCon tt targs (name,Fun.Forall tvs ( [] :=> typ)) =
    translateProductType tt tas where
        (tas, tr) = Fun.unwindType typ'
        subst = Subst (zip tvs targs)
        typ' = apply subst typ

translateDCon tt targs (name,s) = error("translateDCon: "++str name++":"++str s
                            ++"is a constrained constructor")
translateProductType :: TypeTable -> [Fun.Type] -> Core.Type
translateProductType _ [] = Core.TUnit
translateProductType tt ts = foldr1 Core.TPair (map (translateType tt) ts)

translateProduct :: [Fun.TcExpr] -> Translation Core.Expr
translateProduct [e] = translateExp e
translateProduct (e:es) = do
    (code, coreE) <- translateExp e
    (codes, coreEs) <- translateProduct es
    pure (code ++ codes, Core.EPair coreE coreEs)
translateProduct [] = pure ([], Core.EUnit)

translateConApp :: Name -> [Fun.TcExpr] -> Translation Core.Expr
translateConApp c es = do
    (code, conArg) <- translateProduct es
    allCons <- reader (conSiblings c)
    pure (code, encodeCon c allCons conArg)

encodeCon :: Name -> [ConInfo] ->  Core.Expr -> Core.Expr
encodeCon c [con] e
  | c == fst con = e
  | otherwise = error ("encodeCon: constructor "++str c ++ " not found in "++str con)
encodeCon c (con:cons) e
  | c == fst con = Core.EInl e
  | otherwise = Core.EInr e'
  where e' = encodeCon c cons e


-- translate pattern arguments to a substitution, e.g.
-- p@(Just x) ~> [x -> p]
-- p@(Pair x y) ~> [x -> fst p, y -> snd p]
-- p@(Triple x y z) ~> [x -> fst p, y -> fst (snd p), z -> snd (snd p)]
translatePatArgs :: Core.Expr -> [Fun.Arg] -> VSubst
translatePatArgs p = Map.fromList . go p where
    go _ [] = []
    go p [a] = [(Fun.argName a, p)]
    go p (a:as) = let (p1, p2) = (Core.EFst p, Core.ESnd p) in
        (Fun.argName a, p1) : go p2 as
