module Language.Yul where

import Common.Pretty

newtype Yul = Yul { yulStmts :: [YulStatement] }
instance Show Yul where show = render . pretty
instance Show YulStatement where show = render . pretty
instance Show YulExpression where show = render . pretty
instance Show YulLiteral where show = render . pretty

type Name = String
type YArg = Name
type YReturns = Maybe [Name]
pattern YNoReturn :: Maybe a
pattern YNoReturn = Nothing
pattern YReturns :: a -> Maybe a
pattern YReturns a = Just a
pattern YulAlloc :: Name -> YulStatement
pattern YulAlloc name = YulLet [name] Nothing
pattern YulAssign1 :: Name -> YulExpression -> YulStatement
pattern YulAssign1 name expr = YulAssign [name] expr

data YulStatement
  = YulBlock [YulStatement]
  | YulFun String [YArg] YReturns [YulStatement]
  | YulLet [String] (Maybe YulExpression)
  | YulAssign [String] YulExpression
  | YulIf YulExpression [YulStatement]
  | YulSwitch YulExpression [(YulLiteral, [YulStatement])] (Maybe [YulStatement])
  | YulForLoop [YulStatement] YulExpression [YulStatement] [YulStatement]
  | YulBreak
  | YulContinue
  | YulLeave
  | YulComment String
  | YulExpression YulExpression

data YulExpression
  = YulCall String [YulExpression]
  | YulIdentifier String
  | YulLiteral YulLiteral

data YulLiteral
  = YulNumber Integer
  | YulString String
  | YulTrue
  | YulFalse

yulInt :: Integral i => i -> YulExpression
yulInt = YulLiteral . YulNumber . fromIntegral

yulBool :: Bool -> YulExpression
yulBool True = YulLiteral YulTrue
yulBool False = YulLiteral YulFalse

instance Pretty Yul where
  pretty (Yul stmts) = vcat (map pretty stmts)

instance Pretty YulStatement where
  pretty (YulBlock stmts) =
    lbrace
      $$ nest 4 (vcat (map pretty stmts))
      $$ rbrace
  pretty (YulFun name args rets stmts) =
    text "function"
      <+> text name
      <+> prettyargs
      <+> prettyrets rets
      <+> lbrace
      $$ nest 4 (vcat (map pretty stmts))
      $$ rbrace
    where
        prettyargs = parens (hsep (punctuate comma (map text args)))
        prettyrets Nothing = empty
        prettyrets (Just rs) = text "->" <+> (hsep (punctuate comma (map text rs)))
  pretty (YulLet vars expr) =
    text "let" <+> hsep (punctuate comma (map text vars))
               <+> maybe empty (\e -> text ":=" <+> pretty e) expr
  pretty (YulAssign vars expr) = hsep (punctuate comma (map text vars)) <+> text ":=" <+> pretty expr
  pretty (YulIf cond stmts) = text "if" <+> parens (pretty cond) <+> pretty (YulBlock stmts)
  pretty (YulSwitch expr cases def) =
    text "switch"
      <+> (pretty expr)
      $$ nest 4 (vcat (map (\(lit, stmts) -> text "case" <+> pretty lit <+> pretty (YulBlock stmts)) cases))
      $$ maybe empty (\stmts -> text "default" <+> pretty (YulBlock stmts)) def
  pretty (YulForLoop pre cond post stmts) =
    text "for" <+> braces (hsep  (map pretty pre))
               <+> pretty cond
               <+> hsep (map pretty post) <+> pretty (YulBlock stmts)
  pretty YulBreak = text "break"
  pretty YulContinue = text "continue"
  pretty YulLeave = text "leave"
  pretty (YulComment c) = text "/*" <+> text c <+> text "*/"
  pretty (YulExpression e) = pretty e

instance Pretty YulExpression where
  pretty (YulCall name args) = text name >< parens (hsep (punctuate comma (map pretty args)))
  pretty (YulIdentifier name) = text name
  pretty (YulLiteral lit) = pretty lit

instance Pretty YulLiteral where
  pretty :: YulLiteral -> Doc
  pretty (YulNumber n) = integer n
  pretty (YulString s) = doubleQuotes (text s)
  pretty YulTrue = text "true"
  pretty YulFalse = text "false"

{- | wrap a Yul chunk in a Solidity function with the given name
   assumes result is in a variable named "_result"
-}
wrapInSolFunction :: Pretty a => Name -> a -> Doc
wrapInSolFunction name yul = text "function" <+> text name <+> prettyargs <+> text " public pure returns (uint256 _wrapresult)" <+> lbrace
  $$ nest 2 assembly
  $$ rbrace
  where
    assembly = text "assembly" <+> lbrace
      $$ nest 2 (pretty yul)
      $$ rbrace
    prettyargs = parens empty

wrapInContract :: Name -> Name -> Doc -> Doc
wrapInContract name entry body = empty
  $$ text "// SPDX-License-Identifier: UNLICENSED"
  $$ text "pragma solidity ^0.8.23;"
  $$ text "import {console,Script} from \"lib/stdlib.sol\";"
  $$ text "contract" <+> text name <+> text "is Script"<+> lbrace
  $$ nest 2 run
  $$ nest 2 body
  $$ rbrace

  where
    run = text "function run() public view" <+> lbrace
      $$ nest 2 (text "console.log(\"RESULT --> \","<+> text entry >< text ");")
      $$ rbrace $$ text ""
