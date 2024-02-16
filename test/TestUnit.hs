{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Syntax
import qualified ISyntax as I
import Types
import Desugar
import Checker(typeCheck, verboseCheck, tiDecl, tiProg, tiExpr, generalize)
import TCM
import Control.Exception
import Control.Monad
import System.Exit

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
  checkExpr [expr| let id = \x -> x in id id |]
  -- checkExpr [expr| \x -> x x |]
  -- checkExpr [expr| add 1 false |]
  -- testCheck [expr| add 2 3 |]
  -- checkExpr [expr| let n5 = add 2 3 in n5 |]
  -- checkExpr [expr| foldr cons nil |]
  -- checkExpr [expr| foldr add 0 |]
  writeln "-----------------------------------------------------------------------------"


report :: ErrorCall -> IO ()
report (ErrorCall s) = putStrLn ("ERR: " ++ s)

schemeOf :: Expr -> TCM Scheme
schemeOf expr = tiExpr (desugar expr) >>= generalize

checkExpr :: Expr -> IO ()
-- checkExpr exp = typeCheck (desugar exp)
checkExpr exp = check schemeOf exp >> return ()
  {-
    do
  res <- try $ typeCheck exp
  case res of
    Left err -> putStrLn ("Error in "++showExpr exp) >> report err
    Right _ -> pure ()
-}
checkDecl :: Decl -> IO TcState
checkDecl = check  (tiDecl . desugar)

check :: (Print e, Show t) => (e -> TCM t)-> e -> IO (TcState)
check checker e = check' printTree checker e

check' :: (Show t) => (e->String) -> (e -> TCM t)-> e -> IO (TcState)
check' toString checker e = do
  let (res, state) = runTCM (checker e)
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right t -> putStrLn $ (toString e) ++ " :: " ++ show t
  pure state
       
main = try run >>= \case
  Left e -> report e >> exitFailure
  Right _ -> putStrLn "" >> exitSuccess

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
