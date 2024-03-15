class a : Eq { eq : a -> a -> Bool };

instance Int : Eq;
instance Bool : Eq;
instance a : Eq => List[a] : Eq;
instance (a:Eq, b:Eq) => Pair[a,b] : Eq;

f a = eq (pair a a) (pair a a)