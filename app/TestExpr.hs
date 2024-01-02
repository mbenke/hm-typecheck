{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module TestExpr where
import MLExpr
test1 :: Expr
test1  = [expr| \x -> \y -> x y |]
