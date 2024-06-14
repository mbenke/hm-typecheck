type Triple[a,b,c] = Triple[a,b,c];

// add :Int -> Int -> Int;
// add3 x y z = add(add x y) z;
// sumt t = case t of { Triple a b c -> add3 a b c };
// main = sumt (Triple 1 20 21);
// TODO: function ordering
asel t = case t of { Triple a b c -> c};
main = asel (Triple 1 20 21);