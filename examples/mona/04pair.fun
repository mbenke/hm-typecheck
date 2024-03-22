type Pair[a,b] = Pair[a,b];
primFst : Pair[a,b] -> a;
fst p = primFst p;
main = fst (Pair 1 True);
