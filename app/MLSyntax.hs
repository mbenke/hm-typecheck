module MLSyntax where

type Name = String    
name = id

type Expr = Exp
data Exp = EInt Int | EVar Name
         | ELam Name Exp | EApp Exp Exp
         | ELet Name Exp Exp -- let x=e1 in e2
         | ERec Defs Exp

type Def = (Name, Exp)
type Defs = [Def]

-- Przykładowe lambda-termy


mkI :: Exp
mkI = mkI' "x"
mkI' :: Name -> Exp
mkI' n = ELam n $ EVar n

mkK :: Exp
mkK = ELam "x" $ ELam "y" $ EVar "x"


mkS :: Exp
mkS = ELam "x" $ ELam "y" $ ELam "z"
          $ EApp
             (EApp (EVar "x") (EVar "z"))
             (EApp (EVar "y") (EVar "z"))

k7 :: Exp
k7 = EApp mkK (EInt 7)

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp
mkOmega = ELam "x" $ EApp (EVar "x") (EVar "x")

-- wariant typowalny w ML

letOmega :: Exp
letOmega = ELet "x" (mkI' "z") (EApp (EVar "x") (EVar "x"))
-- Show
showName :: Name -> ShowS
showName = showString

instance Show Exp where
  showsPrec d (EVar n) = showString n
  showsPrec d (EInt i) = showsPrec 10 i
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec (ap_prec) e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
          where ap_prec = 10

  showsPrec d (ELam n e) = showParen (d > lam_prec) $
             showString ("\\"++n++"->") .
             showsPrec (lam_prec) e
          where lam_prec = 1

  showsPrec d (ELet n e1 e2) = showParen (d > let_prec) $
    showString "let " .
    showString n .
    showString " = " .
    showsPrec 0 e1 .
    showString " in " .
    showsPrec let_prec e2
    where let_prec = 1
