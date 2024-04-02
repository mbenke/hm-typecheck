module Main where
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when, forM_ )

-- import AbsFun   ()
import qualified AbsFun as C
import LexFun   ( Token, mkPosToken )
import ParFun   ( pProg, myLexer )
import PrintFun ( printTree)
import Desugar
import ISyntax
import TCM
import Checker

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s


runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v


run :: Verbosity -> String -> IO ()
run verbosity input =
  case pProg tokens of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV verbosity "Tokens:"
      mapM_ (putStrV verbosity . showPosToken . mkPosToken) tokens
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      -- let prog@(Prog decls) = desugar tree
      -- forM_ decls print
      checkProg' (verbosity > 1) tree
      exitSuccess
  where
  tokens = myLexer input
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Check stdin verbosely."
    , "  (files)         Check content of files silently."
    , "  -v (files)      Verbose mode. Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    ["--help"] -> usage
    ["-"]         -> getContents >>= run 2
    "-v":fs    -> mapM_ (runFile 2) fs
    fs         -> mapM_ (runFile 0) fs

checkProg = checkProg' False
vcheckProg = checkProg' True

checkProg' :: Bool -> C.Prog -> IO ()
checkProg' verbose prog = do
  let (res, state) = runTCM (tiProg (desugar prog))
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right t -> do
        putStrLn(printTree prog)
        let env = tcsEnv state
        let withPrims = False
        writeln ""
        writeln (showEnv withPrims env)
        writeln "------------\nSpecialised:\n------------"
        writeln (showSpecTable(tcsSpec state))
        writeln "------------\nResolutions:\n------------"
        writeln (showREnv(tcsREnv state))
  when verbose $ do
             let history = reverse (tcsLog state)
             writeln "------------\nHistory:\n------------"
             mapM_ putStrLn history

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
