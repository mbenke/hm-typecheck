{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import MLExpr
import MLTypes
import HMChecker(typeCheck, verboseCheck, tiDecl, tiProg)
import TCM
import Control.Exception
import Control.Monad
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

decl1 :: Decl
decl1 = [decl| id = \x -> x |]

prog1 = [prog|
     len = foldr (\ c n -> add 1 n) 0;
     sum = foldr add 0;
     elem x xs = foldr (\y r -> or (eq x y) r) false xs;
     f1 = elem 1 nil;
     f2 x xs = or (eq x (head xs)) (eq (tail xs) nil);
     f3 x xs = or (elem x xs) (eq (tail xs) nil);
    |]

prog2 :: Prog
prog2 = [prog|
    mi = newMRef 42;
    x1 = load mi;
    x2 = load siExample;
    f3 x = load (newMRef (add x 1))
    |]


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
  checkProg prog1
  writeln "-----------------------------------------------------------------------------"
  checkProg prog2
  writeln "-----------------------------------------------------------------------------"
  -- writeln "Error example:"
  -- checkProg [prog| sum = foldr add 0; bad = sum false  |]


report :: ErrorCall -> IO ()
report (ErrorCall s) = putStrLn ("ERR: " ++ s)

checkExpr :: Expr -> IO ()
checkExpr exp = typeCheck exp
  {-
    do
  res <- try $ typeCheck exp
  case res of
    Left err -> putStrLn ("Error in "++showExpr exp) >> report err
    Right _ -> pure ()
-}
checkDecl :: Decl -> IO TcState
checkDecl = check tiDecl

checkProg = checkProg' False
vcheckProg = checkProg' True

checkProg' verbose prog = do
  let (res, state) = runTCM (tiProg prog)
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right t -> do
        putStrLn $ (printTree prog)
        let env = tcsEnv state
        let withPrims = False
        writeln ""
        writeln (showEnv withPrims env)
  when verbose $ do
             let history = reverse (tcsLog state)
             hrule
             putStrLn "History:"
             mapM_ putStrLn history

check :: (Print e, Show t) => (e -> TCM t)-> e -> IO (TcState)
check c e = do
  let (res, state) = runTCM (c e)
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right t -> putStrLn $ (printTree e) ++ " :: " ++ show t
  pure state


main = try run >>= \case
  Left e -> report e
  Right _ -> putStrLn ""

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
