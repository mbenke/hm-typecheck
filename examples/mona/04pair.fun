type Pair[a,b] = Pair[a,b];
// primFst : Pair[a,b] -> a;
fst p = case p of { Pair a b -> a };
// main = fst (Pair 1 True);
main = fst (Pair 1 0);
