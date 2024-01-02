{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import MLExpr
import MLTypes
import HMChecker(typeOf, typeCheck, verboseCheck)
import Control.Exception

-- QoL helpers
vap :: Expr -> [Expr] -> Expr
vap e = foldl EApp e


-- examples
expS :: Expr
expS = [expr| \x -> \y -> \z -> x z(y z) |]

expK :: Expr
expK = [expr| \x -> \y -> x |]

expSkk :: Expr
expSkk = vap expS [expK, expK]

run = do
  checkExpr expS
  checkExpr expSkk
  -- checkExpr [expr| \x -> x x |]
  -- checkExpr [expr| add 1 false |]
  -- testCheck [expr| add 2 3 |]
  checkExpr [expr| let n5 = add 2 3 in n5 |]
  checkExpr [expr| let id = \x -> x in id id |]
  checkExpr [expr| foldr cons nil |]
  checkExpr [expr| foldr add 0 |]
  checkExpr [expr| foldr (\c n -> add 1 n) 0 |]
  checkExpr [expr| let sum = foldr (\c  n -> add 1 n) 0 in sum |]
  checkExpr [expr| \x xs -> foldr (\y r -> or (eq x y) r) false xs |]
  checkExpr [expr| let elem = \x xs -> foldr (\y r -> or (eq x y) r) false xs in elem |]
  checkExpr [expr| let elem = \x xs -> foldr (\y r -> or (eq x y) r) false xs in elem 1 nil |]
  checkExpr [expr| \ x xs -> or (eq x (head xs)) (eq (tail xs) nil) |]
  checkExpr [expr| \ x xs -> let elem = \x xs -> foldr (\y r -> or (eq x y) r) false xs in or (elem x xs) (eq (tail xs) nil) |]

  -- test error reporting:
  -- checkExpr undefined

report :: ErrorCall -> IO ()
report (ErrorCall s) = putStrLn ("ERR: " ++ s)

checkExpr :: Expr -> IO ()
checkExpr exp = do
  res <- try $ typeCheck exp
  case res of
    Left err -> putStrLn ("Error in "++showExpr exp) >> report err
    Right _ -> pure ()

main = try run >>= \case
  Left e -> report e
  Right _ -> putStrLn "---------"
