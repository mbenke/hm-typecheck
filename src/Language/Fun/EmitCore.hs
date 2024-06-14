module Language.Fun.EmitCore where
import Language.Core(Core(..))
import Language.Core qualified as Core
import TCM
import Data.Map qualified as Map
import Control.Monad.State(gets)
import Language.Fun.ISyntax(Name, ToStr(..))
import Language.Fun.ISyntax qualified as Fun
import Language.Fun.Types qualified as Fun

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
    warn ["> emitSpec ", name, " : ", str typ]
    warn ["! emitSpec: allArgs=", str allArgs]
    let coreArgs = map translateArg allArgs
    let (coreBody, ()) = translateBody lambody
    return (Core.SFunction name coreArgs Core.TInt coreBody)

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

type Translation a = ([Core.Stmt], a)

translateBody :: Fun.Expr -> Translation ()
translateBody exp = do
    let (code, result) = translateExp exp
    (code ++ [Core.SReturn result], ())
    


translateExp :: Fun.Expr -> ([Core.Stmt], Core.Expr)
translateExp (Fun.EInt n) = ([], Core.EInt (fromInteger n))
translateExp (Fun.EVar n) = ([], Core.EVar n)
translateExp e@(Fun.EApp f a) = do
    let (g, as) = unwindApp e
    case g of
        Fun.EVar f -> translateExp (Fun.EVapp g as)
        Fun.ECon c -> translateConApp c as
        _ -> error ("translateExp: not implemented for "++str e)
translateExp (Fun.ECase p [Fun.CaseAlt "Pair" [Fun.TArg a aType, Fun.TArg b bType] body]) = do 
    -- FIXME: generalise
    let (pcode, pair) = translateExp p
    let acType = translateType aType
    let bcType = translateType bType
    let project = declare a acType Core.EFst pair ++ declare b bcType Core.ESnd pair
    let (bCode, bResult) = translateExp body
    ([Core.SBlock $ project ++ bCode], bResult) -- FIXME result is local?
    where
        declare :: Name -> Core.Type -> (Core.Expr -> Core.Expr) -> Core.Expr -> [Core.Stmt]
        declare n t op pair = [Core.SAlloc n t, Core.SAssign (Core.EVar n) (op pair)]
translateExp (Fun.EVapp (Fun.EVar f) as) = (code, call) where
    call = Core.ECall f coreArgs
    (codes, coreArgs) = unzip (map translateExp as)
    code = concat codes
translateExp exp = error ("translateExp: not implemented for "++str exp)

unwindApp :: Fun.Expr -> (Fun.Expr, [Fun.Expr])
unwindApp e = go e [] where
    go (Fun.EApp f a) as = go f (a:as)
    go f as = (f, as)

-- translate (monomorphic) datatypes to sums of products
-- constructors are translated to inr/inl chains
-- e.g. data Color = Red | Green | Blue
-- Red -> 0 inr/ 1 inl  | G -> 1/1 | B -> 2/0
-- NB no inr/inl with one constructor, e.g.
-- type Pair a b = Pair a b; Pair -> 0/0

translateTCon :: Name -> [Fun.Type] -> Core.Type
-- hack, FIXME
translateTCon "Pair" [a, b] = Core.TPair (translateType a) (translateType b)

translateProduct :: [Fun.Expr] -> Translation Core.Expr
translateProduct [e] = translateExp e
translateProduct (e:es) = do
    let (code, coreE) = translateExp e
    let (codes, coreEs) = translateProduct es
    (code ++ codes, Core.EPair coreE coreEs)

translateConApp :: Name -> [Fun.Expr] -> Translation Core.Expr
translateConApp c es = do
    let (code, coreEs) = translateProduct es
    (code, coreEs)