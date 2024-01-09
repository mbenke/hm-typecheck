module Prims where
import MLTypes
import MLExpr

primVals :: [(Name, Scheme)]
primVals =
  [ ("undefined", forAll "a" a)
  , ("zero", monotype $ int :-> int :-> int)
  , ("add", monotype $ int :-> int :-> int)
  , ("ifzero", forAll "a" $ int :-> a :-> a :-> a )
  , ("recInt", forAll "a" $ (int :-> a) :-> a :-> int :-> a)
  , ("true" , monotype bool)
  , ("false", monotype bool)
  , ("not", monotype $ bool :-> bool)
  , ("or", bool2)
  , ("and", bool2)
  , ("ifte", forAll "a" $ bool :-> a :-> a :-> a)
  , ("nil", forAll "a" $ list a)
  , ("cons", forAll "a" $ a :-> list a :-> list a)
  , ("foldr", forAll "a b" $ (a :-> b :-> b) :-> b :-> list a :-> b)
  , ("head", forAll "a" $ list a :-> a)
  , ("tail", forAll "a" $ list a :-> list a)
  , ("eq", Forall ["a"] $ [IsIn "Eq" a] :=> a :-> a :-> bool)
  , ("newMRef", forAll "a" $ a :->  memo a)
  , ("load", Forall ["a", "b"] $ [InCls ["a", "b"] "Ref" [b] a] :=> a :-> b)
  , ("siExample", monotype $ TCon "SI" [])
  ] where
  a = TVar "a"
  b = TVar "b"
  bool2 = monotype $ bool :-> bool :-> bool
  list x = TCon "List" [x]
  stack x = TCon "Stack" [x]
  memo x = TCon "Memory" [x]   

primTypes :: [(Name, Int)]
primTypes =
  [ ("Int", 0)
  , ("Bool", 0)
  , ("->",  2)
  , ("List", 1)
  , ("Maybe", 1)
  , ("Either", 2)
  , ("Memory", 1)
  , ("Stack", 1)
  , ("SI", 0)
  ]

primClasses =
  [ ("Eq", eqClassInfo)
  , ("Ref", refClassInfo)
  ]

eqClassInfo = (["eq"],
               [ [] :=> IsIn "Eq" int
               , [] :=> IsIn "Eq" bool
               , [IsIn cEq a] :=> IsIn cEq (list a)
               ])
  where
    a = TVar "a"
    b = TVar "b"
    list x = TCon "List" [x]
    cEq = "Eq"

refClassInfo = (["load"],
               [ [] :=> InCls [] "Ref" [int] (stack int)
               , [] :=> InCls ["a"] "Ref" [a] (memo a)
               , [] :=> InCls [] "Ref" [int] (TCon "SI" [])
               ])
  where
    a = TVar "a"
    b = TVar "b"
    stack x = TCon "Stack" [x]
    memo x = TCon "Memory" [x]   
