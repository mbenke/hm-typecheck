module Language.Fun.Prims where
import Language.Fun.Types
import Language.Fun.ISyntax

primVals :: [(Name, Scheme)]
primVals =
  [ ("undefined", forAll "a" a)
  , ("zero", monotype $ int :-> int :-> int)
  , ("add", monotype $ int :-> int :-> int)
  , ("sub", monotype $ int :-> int :-> int)
  , ("even", monotype $ int :-> bool)
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
  , ("pair", forAll "a b" $ a :-> b :-> pair a b)
  , ("fst", forAll "a b" $ pair a b :-> a)
  , ("snd", forAll "a b" $ pair a b :-> b)
  , ("eq", Forall ["a"] $ [IsIn "Eq" a] :=> a :-> a :-> bool)
  , ("newMRef", forAll "a" $ a :->  memo a)
  -- Constructors for primitive types
  , ("Unit", monotype unit)
  , ("True", monotype bool)
  , ("False", monotype bool)
  , ("Nil", forAll "a" $ list a)
  , ("Cons", forAll "a" $ a :-> list a :-> list a)
  -- methods for class Ref
  , ("load", Forall ["a", "b"] $ [InCls "Ref" [b] a] :=> a :-> b)
  , ("store", Forall ["a", "b"] $ [InCls "Ref" [b] a] :=> a :-> b :-> unit)
  ] where
  a = TVar "a"
  b = TVar "b"
  unit = TCon "Unit" []
  bool2 = monotype $ bool :-> bool :-> bool
  list x = TCon "List" [x]
  stack x = TCon "Stack" [x]
  memo x = TCon "Memory" [x]
  pair x y = TCon "Pair" [x, y]

primTypes :: [(Name, (Int, [(String, Scheme)]))]
primTypes =
  [ ("Int", (0, []))
  , ("Unit", (0, [("Unit", unit)]))
  , ("Bool", (0, [("False",tbool), ("True",tbool)]))
  , ("->",  (2, []))
  , ("List", (1,
     [ ("Nil", forAll "a" (list a))
     , ("Cons", forAll "a" (a :-> list a :-> list a))]))
--  , ("Maybe", (1, ["Nothing", "Just"]))
--   , ("Either", 2)
{-
  , ("Pair", (2)
  , ("Memory", 1)
  , ("Stack", 1)
  , ("SI", 0)
-}
  ] where
    a = TVar "a"
    b = TVar "b"
    unit = monotype $ TCon "Unit" []
    tbool = monotype bool
    list x = TCon "List" [x]
    pair x y = TCon "Pair" [x, y]

type ClassInfo = (Int, [String])

primClasses =
  [ ("Eq", eqClassInfo)
  , ("Ref", refClassInfo)
  -- class a:IndexAccessible[indexType, memberType]
  -- , ("IndexAccessible", indexClassInfo)
  , ("MemoryBaseType", mbtClassInfo)
  ]

eqClassInfo :: ClassInfo
eqClassInfo = (0, ["eq"])
eqInstances = [ [] :=> IsIn "Eq" int
               , [] :=> IsIn "Eq" bool
               , [IsIn cEq a] :=> IsIn cEq (list a)
              ] where
  a = TVar "a"
  b = TVar "b"
  list x = TCon "List" [x]
  cEq = "Eq"

refClassInfo = (1, ["load"])
refInstances =
  [ [] :=> InCls "Ref" [int] (stack int)  -- inst Ref(int) (Stack Int)
    -- inst Ref a (Memory a)  becomes
  , [b :~:  a] :=> InCls "Ref" [b] (memo a)  -- (b ~ a) => inst Ref b (Memory a)
  , [b :~: int] :=> InCls "Ref" [b] (TCon "SI" [])
  ]
  where
    a = TVar "a"
    b = TVar "b"
    stack x = TCon "Stack" [x]
    memo x = TCon "Memory" [x]

indexClassInfo = (2, ["indexAccess"])

-- class a:ReadFromMemory => a:MemoryBaseType
mbtClassInfo = (0, ["stride"])
