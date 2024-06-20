module Main where
import Exp
import Check
import TCM
typ1 :: Typ
typ1 = TInt  :-> TInt
typ21 :: Typ
typ21 = TInt :-> typ1
typ22 :: Typ
typ22 = typ1 :-> TInt

expK :: Exp
expK = Abs "x" (Abs "y" (Var "x"))

expK2 :: Exp
expK2 = Abs "x" (Abs "y" (Ann (Var "x") (TVar "t")))

expS :: Exp
expS = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

expSKK :: Exp
expSKK = App (App expS expK) expK

main :: IO ()
main = do 
    print typ21
    print typ22
    print expK
    print expS
    putStr "SKK = "
    print expSKK
    print $ evalTCM (doInfer expK)
    print $ evalTCM (doInfer expS)
    print $ evalTCM (doInfer expSKK)