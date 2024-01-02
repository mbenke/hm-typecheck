module MLTypes where
import Data.List(intercalate)
import Text.Show

import MLExpr

infixr 5 :->
infix 2 :=>

data Qual t = [Pred] :=> t deriving Eq
data Pred = IsIn Class Type deriving Eq
type Class = String

data Type = TCon String [Type] {- | Type :-> Type -} | TVar Tyvar
  deriving Eq

desugarT :: Type -> Type
desugarT t = t

pattern TInt = TCon "Int" []
pattern (:->) a b = TCon "->" [a,b]
int :: Type
int = TInt

bool :: Type
bool = TCon "Bool" []

instance Show Type where
  showsPrec d (TVar t) = showString t
  showsPrec d (u :-> v) = showParen (d > arr_prec) $
             showsPrec (arr_prec+1) u .
             showString " -> "       .
             showsPrec arr_prec v
          where arr_prec = 5
  showsPrec d (TCon n []) = showString n
  showsPrec d (TCon n ts) = showString n . showParen True (showString r) where
    args = map show ts
    r = intercalate ", " args

instance Show t => Show (Qual t) where
  showsPrec d ([] :=> t) = showsPrec d t
  showsPrec d ([p] :=> t) = showsPrec 2 p . showString " => " . showsPrec 3 t
  showsPrec d (ps :=> t) = (showParen many showps) . showString " => " . showsPrec 3 t
    where
      many = length ps > 2
      showps = showString $ intercalate ", " (map show ps)

instance Show Pred where
  showsPrec d (IsIn c t) = (showClass c++) . (' ':) . showsPrec 6 t

showClass name = name

type Tyvar = String
data Scheme = Forall [Tyvar] (Qual Type) 

monotype :: Type -> Scheme
monotype t = Forall [] ([] :=> t)

forAll :: String -> Type -> Scheme
forAll s t = Forall (words s) ([] :=> t)

-- deriving instance Show Scheme

instance Show Scheme where
    showsPrec d (Forall [] t) = shows t    
    showsPrec d (Forall as t) = showString "forall ". showStrings as . showString "." . shows t

showStrings :: [String] -> ShowS
showStrings [] = id
showStrings [s] = showString s                 
showStrings (s:ss) = showString s. showString " " . showStrings ss
