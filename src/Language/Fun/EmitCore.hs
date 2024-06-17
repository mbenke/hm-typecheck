module Language.Fun.EmitCore where
import Language.Core(Core(..))
import Language.Core qualified as Core
import TCM
import Data.Map qualified as Map
import Control.Monad.Reader.Class
import Control.Monad.State(gets)
import Language.Fun.ISyntax(Name, ToStr(..), Expr(..))
import Language.Fun.ISyntax qualified as Fun
import Language.Fun.Types qualified as Fun


type VSubst = Map.Map Name Core.Expr
emptyVSubst :: VSubst
emptyVSubst = Map.empty

data  ECEnv = ECEnv { ecSubst :: VSubst, ecTT :: TypeTable}

extendECEnv :: VSubst -> ECEnv -> ECEnv
extendECEnv subst env = env { ecSubst = ecSubst env <> subst}

-- readConstructorPosition :: Name -> (Int, Int)

emitCore :: TCM Core
emitCore = do
    specTable <- gets tcsSpec
    cores <- mapM emitSpecs (Map.toList specTable)
    pure (Core (concat cores))

emitSpecs :: (Name, [Specialisation]) -> TCM [Core.Stmt]
emitSpecs (name, specs) = mapM (emitSpec name) specs

emitSpec :: Name -> Specialisation -> TCM Core.Stmt
emitSpec origName (name, typ, args, body) = do
    let (lambody, lamas) = stripLambda body
    let allArgs = args ++ lamas
    -- warn ["> emitSpec ", name, " : ", str typ]
    -- warn ["! emitSpec: allArgs=", str allArgs]
    -- warn ["! emitSpec: lambody=", str lambody]
    let coreArgs = map translateArg allArgs
    coreBody <- translateBody lambody
    return (Core.SFunction name coreArgs Core.TInt coreBody)


type Translation a = ECEnv -> ([Core.Stmt], a)

translateBody :: Fun.Expr -> TCM [Core.Stmt]
translateBody exp = do
    typeTable <- gets tcsTT
    let env = ECEnv emptyVSubst typeTable
    let (code, result) = translateExp exp env
    return (code ++ [Core.SReturn result])

translateExp :: Fun.Expr -> Translation Core.Expr
translateExp (Fun.EInt n) = pure ([], Core.EInt (fromInteger n))
translateExp (Fun.EVar n) = do
    replace <- reader (Map.lookup n . ecSubst)
    case replace of
        Just e -> pure ([], e)
        Nothing -> pure ([], Core.EVar n)
translateExp e@(Fun.EApp f a) = do
    let (g, as) = unwindApp e
    case g of
        Fun.EVar f -> translateExp (Fun.EVapp g as)
        Fun.ECon c -> translateConApp c as
        _ -> error ("translateExp: not implemented for "++str e)

translateExp (Fun.EVapp (Fun.EVar f) as) = do
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


translateExp (Fun.ECase (ETyped p typ) [alt]) = do
    (pcode, pval) <- translateExp p
    (bcode, bval) <- translateAlt pval alt
    pure (pcode ++ bcode, bval)

translateExp (Fun.ECase  p [alt]) = error "translateExp: case with untyped scrutinee"
translateExp exp = error ("translateExp: not implemented for "++str exp)

translateAlt :: Core.Expr -> Fun.CaseAlt -> Translation Core.Expr
translateAlt pval (Fun.CaseAlt con args body) = do
    let pvars = translatePatArgs pval args
    local (extendECEnv pvars) $ translateExp body

unwindApp :: Fun.Expr -> (Fun.Expr, [Fun.Expr])
unwindApp e = go e [] where
    go (Fun.EApp f a) as = go f (a:as)
    go f as = (f, as)

stripLambda :: Fun.Expr -> (Fun.Expr, [Fun.Arg])
stripLambda (Fun.ELam args body) = let (b, as) = stripLambda body in (b, args ++ as)
stripLambda e = (e, [])

translateArg :: Fun.Arg -> Core.Arg
translateArg (Fun.TArg n t) = Core.TArg n (translateType t)
translateArg (Fun.UArg n) = error("translateArg: UArg "++n)

translateType :: Fun.Type -> Core.Type
translateType Fun.TInt = Core.TInt
translateType Fun.TBool = Core.TBool
translateType Fun.TUnit = Core.TUnit
translateType (Fun.TCon name tas) = translateTCon name tas

-- translate (monomorphic) datatypes to sums of products
-- constructors are translated to inr/inl chains
-- e.g. data Color = Red | Green | Blue
-- Red -> 0 inr/ 1 inl  | G -> 1/1 | B -> 2/0
-- NB no inr/inl with one constructor, e.g.
-- type Pair a b = Pair a b; Pair -> 0/0

translateTCon :: Name -> [Fun.Type] -> Core.Type
translateTCon con tas = translateProductType tas

translateProductType :: [Fun.Type] -> Core.Type
translateProductType [] = Core.TUnit
translateProductType ts = foldr1 Core.TPair (map translateType ts)

translateProduct :: [Fun.Expr] -> Translation Core.Expr
translateProduct [e] = translateExp e
translateProduct (e:es) = do
    (code, coreE) <- translateExp e
    (codes, coreEs) <- translateProduct es
    pure (code ++ codes, Core.EPair coreE coreEs)

translateConApp :: Name -> [Fun.Expr] -> Translation Core.Expr
translateConApp c es = do
    (code, coreEs) <- translateProduct es
    pure (code, coreEs)


-- translate pattern arguments to a substitution, e.g.
-- p@(Just x) ~> [p->p]
-- p@(Pair x y) ~> [x -> fst p, y -> snd p]
-- p@(Triple x y z) ~> [x -> fst p, y -> fst (snd p), z -> snd (snd p)]
translatePatArgs :: Core.Expr -> [Fun.Arg] -> VSubst
translatePatArgs p = Map.fromList . go p where
    go _ [] = []
    go p [a] = [(Fun.argName a, p)]
    go p (a:as) = let (p1, p2) = (Core.EFst p, Core.ESnd p) in
        (Fun.argName a, p1) : go p2 as