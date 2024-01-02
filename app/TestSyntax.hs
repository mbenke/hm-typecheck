{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- import MLSyntax
import MLExpr
import MLTypes
import MLChecker(typeOf, typeCheck, testCheck)


-- QoL helpers    
vap :: Expr -> [Expr] -> Expr
vap e = foldl EApp e


-- examples
expS :: Expr
expS = [expr| \x -> \y -> \z -> x z(y z) |]
-- expS = mkS
       
expK :: Expr
expK = [expr| \x -> \y -> x |]
-- expK = mkK

expSkk :: Expr
expSkk = vap expS [expK, expK]

testSkk :: IO ()
testSkk = typeCheck expSkk

expSum =  undefined
         
main = do
  testSkk
  
