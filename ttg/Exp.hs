{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Exp where

data Pass = UD | Parsed | Typechecked
data ComPass (c::Pass) where
    ComUD :: ComUD
    ComPs :: ComPs
    ComTc :: ComTc

type ComUD = ComPass 'UD
type ComPs = ComPass 'Parsed
type ComTc = ComPass 'Typechecked

data Typ = TInt | TVar Name | Typ :-> Typ deriving Eq
type Name = String
type Var = Name
type Tyvar = Name

data ExpX x
    = LitX (XLit x) Int
    | VarX (XVar x) Var
    | AnnX (XAnn x) (ExpX x) Typ
    | AbsX (XAbs x) Var (ExpX x)
    | AppX (XApp x) (ExpX x) (ExpX x)
    | ExpX (XExp x)

type family XLit x
type family XVar x
type family XAnn x
type family XAbs x
type family XApp x
type family XExp x

data NoExtField = NoExtField
instance Semigroup NoExtField where _ <> _ = NoExtField
instance Monoid NoExtField where mempty = NoExtField

data DataConCantHappen -- empty data type
-- void :: DataConCantHappen
-- void = error "Attempt to evaluate void"

absurd :: DataConCantHappen -> a
absurd v = case v of {}

type Exp = ExpX ComUD
type instance XLit ComUD = NoExtField
type instance XAnn ComUD = NoExtField
type instance XAbs ComUD = NoExtField
type instance XApp ComUD = NoExtField
type instance XVar ComUD = NoExtField
type instance XExp ComUD = DataConCantHappen

pattern Lit :: Int -> Exp
pattern Lit n <- LitX _ n
  where Lit i = LitX mempty i

pattern Var :: Var -> Exp
pattern Var v <- VarX _ v
  where Var v = VarX mempty v

pattern Ann :: Exp -> Typ -> Exp
pattern Ann e t <- AnnX _ e t
  where Ann e t = AnnX mempty e t

pattern Abs :: Var -> Exp -> Exp
pattern Abs v e <- AbsX _ v e
  where Abs v e = AbsX mempty v e

pattern App :: Exp -> Exp -> Exp
pattern App e1 e2 <- AppX _ e1 e2
  where App e1 e2 = AppX mempty e1 e2

instance Show Typ where
    showsPrec _ TInt = showString "Int"
    showsPrec _ (TVar v) = showString v
    showsPrec d (t1 :-> t2) = showParen (d>1) $ showsPrec 2 t1 . showString " -> " . showsPrec 1 t2

{-
instance Show Exp where
    showsPrec _ (Lit n) = shows n
    showsPrec _ (Var v) = showString v
    showsPrec _ (Ann e t) = showParen True $ shows e . showString " :: " . shows t
    showsPrec d (Abs v e) = showParen (d>0) $ showString ("\\" ++ v ++ " -> ") . shows e
    showsPrec d (App e1 e2) = showParen (d>1) $ showsPrec 1 e1 . showString " " . showsPrec 2 e2
    showsPrec _ (ExpX v) =  case v of {}
    showsPrec _ _ = error "unreachable"
-}
showsTyp :: Int -> Typ -> ShowS
showsTyp _ TInt = showString "Int"
showsTyp _ (TVar v) = showString v
showsTyp d (t1 :-> t2) = showParen (d>arr_prec) $ showsTyp 2 t1 . showString " -> " . showsTyp arr_prec t2 where arr_prec = 1

showsExpX :: Int -> (XExp x -> ShowS) ->  ExpX x -> ShowS
showsExpX _ _ (LitX _ n) = shows n
showsExpX _ _ (VarX _ v) = showString v
showsExpX d p (AnnX _ e t) = showParen (d > prec_ann) $ showsExpX 0 p e . showString " :: " . shows t where prec_ann = 0
showsExpX d p (AbsX _ v e) = showParen (d>lam_prec) $ showString ("\\" ++ v ++ " -> ") . showsExpX lam_prec p e where lam_prec = 1
showsExpX d p (AppX _ e1 e2) = showParen (d>app_prec) $ showsExpX app_prec p e1 . showString " " . showsExpX (app_prec+1) p e2 where app_prec = 10
showsExpX _ p (ExpX v) = p v

instance Show Exp where
    showsPrec d e = showsExpX d absurd e

type ExpTc = ExpX ComTc
type instance XLit ComTc = NoExtField
type instance XAnn ComTc = NoExtField
type instance XAbs ComTc = Typ
type instance XApp ComTc = NoExtField
type instance XVar ComTc = Typ
type instance XExp ComTc = DataConCantHappen

instance Show (ExpX ComTc) where
    showsPrec _ (LitX _ n) = shows n
    showsPrec _ (VarX _ v) = showString v
    showsPrec d (AbsX t v e) = showParen (d>lam_prec) $
        showString "\\" . showString v . showString " :: " . shows t . showString " -> " . shows e
        where lam_prec = 1
    showsPrec d (AnnX _ e t) = showParen (d > ann_prec) $ shows e . showString " :: " . shows t where ann_prec = 0
    showsPrec d (AppX _ e1 e2) = showParen (d>app_prec) $
        showsPrec app_prec e1 . showString " " . showsPrec (app_prec+1) e2
        where app_prec = 10
    showsPrec _ (ExpX v) =  case v of {}