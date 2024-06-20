module Main where
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when, forM_ )
import qualified Data.Map as Map

-- import AbsFun   ()
import qualified AbsFun as C
import LexFun   ( Token, mkPosToken )
import ParFun   ( pProg, myLexer )
import PrintFun ( printTree)
import Desugar
import Language.Fun.ISyntax
-- import Language.Fun.Checker
import Language.Fun.Typecheck
import Language.Fun.EmitCore
import TCM
import Language.Fun.Specialise
import Language.Core(Core)
import Language.Core qualified as Core

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
vcheckProg :: C.Prog -> IO ()
vcheckProg = checkProg' True

processProg :: Prog -> TCM (Maybe Core)
processProg prog@(Prog decls) = do
  tcp <- tcProg prog
  tld <- buildTLD decls
  warn ["Typechecked program:"]
  warn [show tcp]
  warn ["---------------------------------"]
  case Map.lookup entrypoint tld of
    Nothing -> return Nothing
    Just def -> do
      withLogging $ specialiseEntry entrypoint
      Just <$> emitCore
  where
    entrypoint = "main"

checkProg' :: Bool -> C.Prog -> IO ()
checkProg' verbose prog = do
  let (res, state) = runTCM (processProg (desugar prog))
  case res of
    Left err -> putStrLn "Error: " >> putStrLn err
    Right mcore -> do
        putStrLn(printTree prog)
        let env = tcsEnv state
        let withPrims = False
        writeln ""
        writeln (showEnv withPrims env)
        writeln "------------\nResolutions:\n------------"
        writeln (showREnv(tcsREnv state))
        writeln "------------\nSpecialised:\n------------"
        writeln (showSpecTable(tcsSpec state))
        case mcore of
          Nothing -> return ()
          Just core -> do
            writeln "------------\nCore:\n------------"
            writeln (show core)
            writeFile "output.core" (show core)
  when verbose $ do
             let history = reverse (tcsLog state)
             writeln "------------\nHistory:\n------------"
             mapM_ putStrLn history

writeln = putStrLn
hrule = writeln hruleStr
hruleStr = replicate 77 '-'
