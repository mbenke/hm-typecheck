class a : Eq { eq : a -> a -> Bool }; 
instance Int : Eq;
instance Bool : Eq;
instance a : Eq => List[a] : Eq;

// Cannot have instance for nested pairs and regular pairs at once:
// instance (a:Eq, b:Eq) => Pair[a,b] : Eq;
// Error:  instance (Eq a, Eq b, Eq c) => Eq (Pair[a, Pair[b, c]]) overlaps (Eq t, Eq u) => Eq (Pair[t, u])

instance (a:Eq, b:Eq, c:Eq) => Pair[a,Pair[b,c]] : Eq;
nestedPair = pair 1 (pair 2 3);
nestedEq = eq nestedPair nestedPair;