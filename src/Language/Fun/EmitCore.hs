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
    let coreBody = translateBody lambody
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

translateBody :: Fun.Expr -> [Core.Stmt]
translateBody exp = code ++ [Core.SReturn result] where
    (code, result) = translateExp exp

translateExp :: Fun.Expr -> ([Core.Stmt], Core.Expr)
translateExp (Fun.EInt n) = ([], Core.EInt (fromInteger n))
translateExp (Fun.EVar n) = ([], Core.EVar n)
translateExp e@(Fun.EApp f a) = translateExp(Fun.EVapp g as)
    where (g, as) = unwindApp e
translateExp (Fun.EVapp (Fun.EVar f) as) = (code, call) where
    call = Core.ECall f coreArgs
    (codes, coreArgs) = unzip (map translateExp as)
    code = concat codes
translateExp exp = error ("translateExp: not implemented for "++str exp)

unwindApp :: Fun.Expr -> (Fun.Expr, [Fun.Expr])
unwindApp e = go e [] where
    go (Fun.EApp f a) as = go f (a:as)
    go f as = (f, as)