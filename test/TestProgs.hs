{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Syntax
import qualified ISyntax as I
import Types
import Desugar
import Checker( tiDecl, tiProg, schemeOf)
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
  writeln "// prog1:"
  checkProg prog1
  writeln "-----------------------------------------------------------------------------"
  writeln "// prog2:"
  checkProg prog2
  writeln "-----------------------------------------------------------------------------"
  -- checkProg prog3
  writeln "// prog4:"
  checkProg prog4
  writeln "-----------------------------------------------------------------------------"
  -- writeln "Error example:"
  -- checkProg [prog| sum = foldr add 0; bad = sum false  |]


-- Lists, pairs, equality
prog1 = [prog|
    class a : Eq { eq : a -> a -> Bool };

    instance Int : Eq;
    instance Bool : Eq;
    instance a : Eq => List[a] : Eq;
    instance (a:Eq, b:Eq) => Pair[a,b] : Eq;

    type Option[a] = None | Some[a];
    pure = Some;
    instance a:Eq => Option[a] : Eq;

    idInt1 x = x:Int;
    idInt2 (x:Int) = x;

    ifthen c t e = case c of { True -> t; False -> e };

    len xs = case xs of { Nil -> 0; Cons y ys -> add 1 (len ys) };
    f0 = len Nil;
    sum = foldr add 0;

    elem x xs = case xs of {
      Nil -> False;
      Cons y ys -> or (eq x y) (elem  x ys) };
    elem x xs = foldr (\y r -> or (eq x y) r) false xs;
    f1 = elem 1 nil;
    f2 x xs = or (eq x (head xs)) (eq (tail xs) nil);
    f2Int (x:Int) xs = or (eq x (head xs)) (eq (tail xs) nil); //f2 x xs;
    f3 x xs = or (elem x xs) (eq (tail xs) nil);
    pair12 = pair 1 2;
    f4 = elem pair12 nil;
    f5 x  = elem (pair 1 x) nil;
    f6 x ys = elem (pair 1 x) ys;
    f7 = f6 false nil;
    |]

-- References
prog2 :: Prog
prog2 = [prog|
    type Stack[a];
    siExample : Stack[Int];
    instance Stack[Int] : Ref[Int];
    instance SI : Ref[Int];
    instance Memory[a]: Ref[a] ;
    pragma nocoverage;
    instance (ra:Ref[a], rb:Ref[b]) => Pair[ra,rb] : Ref[Pair[a,b]];
    mi = newMRef 42;
    x1 = load mi;
    x2 = load siExample;
    f3 x = load (newMRef (add x 1));
    x4 = pair mi siExample;
    x5 = load x4;

    type Unit = Unit;
    unit : Unit;

    // store : (ref: Ref[a]) => ref -> a -> Unit;
    x6 = store mi x2;

    copy from to = store to (load from);
    x7 = copy siExample mi;

   |]

prog3 = [prog|
     instance Int : Eq;
     instance Bool : Eq;
     instance a : Eq => List[a] : Eq;
     instance (a:Eq, b:Eq) => Pair[a,b] : Eq;
     eq : (a:Eq) => a -> a -> Bool;
     elem x xs = foldr (\y r -> or (eq x y) r) false xs;
     f1 = elem 1 nil;
     f2 x xs = or (eq x (head xs)) (eq (tail xs) nil);
     f3 x xs = or (elem x xs) (eq (tail xs) nil);
     pair12 = pair 1 2;
     f4 = elem pair12 nil;
     f5 x  = elem (pair 1 x) nil;
     f6 x ys = elem (pair 1 x) ys;
     f7 = f6 false nil;
    |]

-- Array indexing
prog4 = [prog|
    instance Memory[a]: Ref[a] ;

    type Itself[a] = Proxy ;
    class a:MemoryBaseType {
      stride : Itself[a] -> Int;
    };

    instance Int : MemoryBaseType;
    class a:IndexAccessible[baseType] {
       indexAccess : a -> Int -> baseType;
    };
    // function indexAccess(array:a, index:uint256) -> baseType;
    // indexAccess : (a:IndexAccessible[baseType]) => a -> Int -> baseType;
    // can this return a reference?
    type MemoryArray[a];
    instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]];

    array : MemoryArray[Int];
    f41 idx = indexAccess array idx;
    x42 = store (f41 1) 42;
    x43 = load (f41 2);
    x44 = store (indexAccess array 1) 42;
    |]

typeCheck :: I.Expr -> IO ()
typeCheck exp = case evalTCM (schemeOf exp) of
                  Left e -> putStrLn "Error: " >> putStrLn e >> putStrLn ""
                  Right t -> putStrLn $ (I.showExpr exp) ++ " :: " ++ show t

report :: ErrorCall -> IO ()
report (ErrorCall s) = putStrLn ("ERR: " ++ s)

checkExpr :: Expr -> IO ()
checkExpr exp = typeCheck (desugar exp)
  {-
    do
  res <- try $ typeCheck exp
  case res of
    Left err -> putStrLn ("Error in "++showExpr exp) >> report err
    Right _ -> pure ()
-}
checkDecl :: Decl -> IO TcState
checkDecl = check  (tiDecl . desugar)

checkProg = checkProg' False
vcheckProg = checkProg' True

checkProg' :: Bool -> Prog -> IO ()
checkProg' verbose prog = do
  let (res, state) = runTCM (tiProg (desugar prog))
  case res of
    Left err -> do
      putStrLn "Error: "
      putStrLn err
      showLog state
      exitFailure
    Right t -> do
        putStrLn (printTree prog)
        let env = tcsEnv state
        let withPrims = False
        writeln "\nTYPES:\n======"
        writeln (showEnv withPrims env)
  when verbose (showLog state)
  where
    showLog state = do
      let history = reverse (tcsLog state)
      hrule
      putStrLn "Log:"
      mapM_ putStrLn history

check :: (Print e, Show t) => (e -> TCM t)-> e -> IO (TcState)
check c e = do
  let (res, state) = runTCM (c e)
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right t -> putStrLn $ (printTree e) ++ " :: " ++ show t
  pure state


main = try run >>= \case
  Left e -> report e >> exitFailure
  Right _ -> putStrLn "" >> exitSuccess

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
