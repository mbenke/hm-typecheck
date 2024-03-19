-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintFun.

module PrintFun where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsFun

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsFun.UIdent where
  prt _ (AbsFun.UIdent i) = doc $ showString i
instance Print AbsFun.LIdent where
  prt _ (AbsFun.LIdent i) = doc $ showString i
instance Print AbsFun.Prog where
  prt i = \case
    AbsFun.Prog decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print AbsFun.Expr where
  prt i = \case
    AbsFun.EBlock stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])
    AbsFun.ETyped expr ctype -> prPrec i 0 (concatD [prt 1 expr, doc (showString ":"), prt 0 ctype])
    AbsFun.ELam args expr -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 args, doc (showString "->"), prt 0 expr])
    AbsFun.ELet lident expr1 expr2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 lident, doc (showString "="), prt 0 expr1, doc (showString "in"), prt 0 expr2])
    AbsFun.EApp expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 2 expr2])
    AbsFun.EMet expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "."), prt 2 expr2])
    AbsFun.EIdx expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    AbsFun.EStar lident -> prPrec i 1 (concatD [doc (showString "*"), prt 0 lident])
    AbsFun.EVar lident -> prPrec i 2 (concatD [prt 0 lident])
    AbsFun.ECon uident -> prPrec i 2 (concatD [prt 0 uident])
    AbsFun.EInt n -> prPrec i 2 (concatD [prt 0 n])

instance Print AbsFun.Arg where
  prt i = \case
    AbsFun.UArg lident -> prPrec i 0 (concatD [prt 0 lident])
    AbsFun.TArg lident ctype -> prPrec i 0 (concatD [doc (showString "("), prt 0 lident, doc (showString ":"), prt 0 ctype, doc (showString ")")])

instance Print [AbsFun.Arg] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsFun.Stmt where
  prt i = \case
    AbsFun.SExpr expr -> prPrec i 0 (concatD [prt 0 expr])
    AbsFun.SAssign expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "="), prt 0 expr2])
    AbsFun.SAlloc lident ctype -> prPrec i 0 (concatD [doc (showString "let"), prt 0 lident, doc (showString ":"), prt 0 ctype])
    AbsFun.SInit lident expr -> prPrec i 0 (concatD [doc (showString "let"), prt 0 lident, doc (showString "="), prt 0 expr])

instance Print [AbsFun.Stmt] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print AbsFun.Decl where
  prt i = \case
    AbsFun.TypeDecl ctype tyderhs -> prPrec i 0 (concatD [doc (showString "type"), prt 0 ctype, prt 0 tyderhs])
    AbsFun.ValDecl lident qtype -> prPrec i 0 (concatD [prt 0 lident, doc (showString ":"), prt 0 qtype])
    AbsFun.ValBind lident args expr -> prPrec i 0 (concatD [prt 0 lident, prt 0 args, doc (showString "="), prt 0 expr])
    AbsFun.InstDecl qpred -> prPrec i 0 (concatD [doc (showString "instance"), prt 0 qpred])
    AbsFun.ClsDecl cpred methods -> prPrec i 0 (concatD [doc (showString "class"), prt 0 cpred, prt 0 methods])
    AbsFun.Pragma lident -> prPrec i 0 (concatD [doc (showString "pragma"), prt 0 lident])

instance Print [AbsFun.Decl] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print AbsFun.Methods where
  prt i = \case
    AbsFun.NoMethods -> prPrec i 0 (concatD [])
    AbsFun.SomeMethods decls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decls, doc (showString "}")])

instance Print AbsFun.QType where
  prt i = \case
    AbsFun.T0Qual ctype -> prPrec i 0 (concatD [prt 0 ctype])
    AbsFun.T1Qual cpred ctype -> prPrec i 0 (concatD [prt 0 cpred, doc (showString "=>"), prt 0 ctype])
    AbsFun.TNQual cpreds ctype -> prPrec i 0 (concatD [doc (showString "("), prt 0 cpreds, doc (showString ")"), doc (showString "=>"), prt 0 ctype])

instance Print AbsFun.QPred where
  prt i = \case
    AbsFun.I0Qual cpred -> prPrec i 0 (concatD [prt 0 cpred])
    AbsFun.I1Qual cpred1 cpred2 -> prPrec i 0 (concatD [prt 0 cpred1, doc (showString "=>"), prt 0 cpred2])
    AbsFun.INQual cpreds cpred -> prPrec i 0 (concatD [doc (showString "("), prt 0 cpreds, doc (showString ")"), doc (showString "=>"), prt 0 cpred])

instance Print AbsFun.TyDeRhs where
  prt i = \case
    AbsFun.EmptyTyDeRhs -> prPrec i 0 (concatD [])
    AbsFun.ConAlts conalts -> prPrec i 0 (concatD [doc (showString "="), prt 0 conalts])

instance Print AbsFun.ConAlt where
  prt i = \case
    AbsFun.ConAlt0 uident -> prPrec i 0 (concatD [prt 0 uident])
    AbsFun.ConAltN uident ctypes -> prPrec i 0 (concatD [prt 0 uident, doc (showString "["), prt 0 ctypes, doc (showString "]")])

instance Print [AbsFun.ConAlt] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print AbsFun.CPred where
  prt i = \case
    AbsFun.PSingle ctype uident -> prPrec i 0 (concatD [prt 0 ctype, doc (showString ":"), prt 0 uident])
    AbsFun.PMulti ctype uident ctypes -> prPrec i 0 (concatD [prt 0 ctype, doc (showString ":"), prt 0 uident, doc (showString "["), prt 0 ctypes, doc (showString "]")])

instance Print [AbsFun.CPred] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsFun.CType where
  prt i = \case
    AbsFun.CTArr ctype1 ctype2 -> prPrec i 0 (concatD [prt 1 ctype1, doc (showString "->"), prt 0 ctype2])
    AbsFun.CTCon uident ctypes -> prPrec i 1 (concatD [prt 0 uident, doc (showString "["), prt 0 ctypes, doc (showString "]")])
    AbsFun.CTCon0 uident -> prPrec i 2 (concatD [prt 0 uident])
    AbsFun.CTVar lident -> prPrec i 2 (concatD [prt 0 lident])

instance Print [AbsFun.CType] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]
