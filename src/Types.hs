module Types where
import Data.List(intercalate)
import Text.Show


infixr 5 :->
infix 2 :=>

data Qual t = [Pred] :=> t deriving Eq

data Pred = InCls { predName :: String, predArgs :: [Type], predTarget :: Type }
          | Type :~: Type
          deriving Eq
pattern IsIn c t = InCls c [] t

type Class = String
type Inst = Qual Pred

data Type = TCon String [Type] {- | Type :-> Type -} | TVar Tyvar
  deriving Eq


pattern TInt = TCon "Int" []
pattern (:->) a b = TCon "(->)" [a,b]
int :: Type
int = TInt

bool :: Type
bool = TCon "Bool" []

unitT = TCon "Unit" []
stackT a = TCon "Stack" [a]

instance Show Type where
  showsPrec d (TVar t) = showString t
  showsPrec d (u :-> v) = showParen (d > arr_prec) $
             showsPrec (arr_prec+1) u .
             showString " -> "       .
             showsPrec arr_prec v
          where arr_prec = 5
  showsPrec d (TCon n []) = showString n
  showsPrec d (TCon n ts) = showParen (d > 10) $
                            showString n .  ('[':) . showString r . (']':) where
    args = map show ts
    r = intercalate ", " args

instance Show t => Show (Qual t) where
  showsPrec d ([] :=> t) = showsPrec d t
  -- showsPrec d ([p] :=> t) = showsPrec 2 p . showString " => " . showsPrec 3 t
  showsPrec d (ps :=> t) = (showParen True showps) . showString " => " . showsPrec 3 t
    where
      many = length ps > 1
      showps = showString $ intercalate ", " (map show ps)

instance Show Pred where
  -- showsPrec d (IsIn c t) = showClass c . (' ':) . showsPrec 11 t
  showsPrec d (IsIn c t) = showsPrec 1 t . showChar ':' . showClass c
  showsPrec d (InCls c as t) = showsPrec 1 t . showChar ':' . showClass c . showList as
  showsPrec d (t :~: u) = showParen (d>0) $ showsPrec 1 t . (" ~ "++) . showsPrec 1 u

showClass name = (name++)

type Tyvar = String
data Scheme = Forall [Tyvar] (Qual Type)

-- funtype [a1, ..., an] r = a1 :-> ... an :-> r
funtype :: [Type] -> Type -> Type
funtype as r = foldr (:->) r as

monotype :: Type -> Scheme
monotype t = Forall [] ([] :=> t)

forAll :: String -> Type -> Scheme
forAll s t = Forall (words s) ([] :=> t)

-- deriving instance Show Scheme

instance Show Scheme where
    showsPrec d (Forall [] t) = shows t
    showsPrec d (Forall as t) = showString "∀". showStrings as . showString "." . shows t

showStrings :: [String] -> ShowS
showStrings [] = id
showStrings [s] = showString s
showStrings (s:ss) = showString s. showString " " . showStrings ss
