{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Syntax
import qualified ISyntax as I
import Types
import Desugar
import Checker(typeCheck, verboseCheck, tiDecl, tiProg)
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
  checkProg prog1
  writeln "-----------------------------------------------------------------------------"
  checkProg prog2
  writeln "-----------------------------------------------------------------------------"
  -- checkProg prog3
  checkProg prog4
  -- writeln "-----------------------------------------------------------------------------"
  -- writeln "Error example:"
  -- checkProg [prog| sum = foldr add 0; bad = sum false  |]


-- Lists, pairs, equality
prog1 = [prog|
     instance Int : Eq;
     instance Bool : Eq;
     instance a : Eq => List[a] : Eq;
     instance (a:Eq, b:Eq) => Pair[a,b] : Eq;

     type Option[a] = None | Some[a];
     pure = Some;
     instance a:Eq => Option[a] : Eq;
     len = foldr (\ c n -> add 1 n) 0;
     sum = foldr add 0;
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

-- References
prog2 :: Prog
prog2 = [prog|
    class ref : Ref[deref] { load : ref -> deref };
    instance Stack[Int] : Ref[Int];
    instance SI : Ref[Int];
    instance Memory[a]: Ref[a] ;
    instance (ra:Ref[a], rb:Ref[b]) => Pair[ra,rb] : Ref[Pair[a,b]];
    mi = newMRef 42;
    x1 = load mi;
    x2 = load siExample;
    f3 x = load (newMRef (add x 1));
    x4 = pair mi siExample;
    x5 = load x4;

    type Unit = Unit;
    unit : Unit;

    store : (ref: Ref[a]) => ref -> a -> Unit;
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
    store : (ref: Ref[a]) => ref -> a -> Unit;

    type Itself[a] = Unit ;
    // class a:ReadFromMemory => a:MemoryBaseType {
    // function stride(_:itself(a)) -> word; }
    stride : (a:MemoryBaseType) => Itself[a] -> Int;
    instance Int : MemoryBaseType;
    // class a:IndexAccessible[baseType]
    // function indexAccess(array:a, index:uint256) -> baseType;
    indexAccess : (a:IndexAccessible[baseType]) => a -> Int -> baseType;
    // can this return a reference?
    type MemoryArray[a];
    instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]];

    array : MemoryArray[Int];
    f8 idx = indexAccess array idx;
    x9 = store (f8 1) 42;
    x10 = load (f8 2);
    |]


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
  Left e -> report e >> exitFailure
  Right _ -> putStrLn "" >> exitSuccess

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
