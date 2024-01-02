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
  tryCheck expS
  tryCheck expSkk
  -- tryCheck [expr| \x -> x x |]
  -- tryCheck [expr| add 1 false |]
  -- testCheck [expr| add 2 3 |]
  tryCheck [expr| let n5 = add 2 3 in n5 |]
  tryCheck [expr| let id = \x -> x in id id |]
  tryCheck [expr| foldr cons nil |]
  tryCheck [expr| foldr add 0 |]
  tryCheck [expr| foldr (\c n -> add 1 n) 0 |]
  tryCheck [expr| let sum = foldr (\c  n -> add 1 n) 0 in sum |]
  tryCheck [expr| \x xs -> foldr (\y r -> or (eq x y) r) false xs |]
  tryCheck [expr| let elem = \x xs -> foldr (\y r -> or (eq x y) r) false xs in elem |]
  tryCheck [expr| let elem = \x xs -> foldr (\y r -> or (eq x y) r) false xs in elem 1 nil |]
  -- test error reporting:
  -- tryCheck undefined

report :: ErrorCall -> IO ()
report (ErrorCall s) = putStrLn ("ERR: " ++ s)

tryCheck :: Expr -> IO ()
tryCheck exp = do
  res <- try $ typeCheck exp
  case res of
    Left err -> putStrLn ("Error in "++showExpr exp) >> report err
    Right _ -> pure ()

main = try run >>= \case
  Left e -> report e
  Right _ -> putStrLn "---------"
