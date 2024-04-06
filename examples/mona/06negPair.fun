type Pair[a,b] = Pair[a,b];
primNegInt : Int -> Int;

class a:Neg { neg : a -> a};
instance Bool:Neg { neg = not };
instance Int:Neg { neg = primNegInt };
instance (a:Neg, b:Neg) => Pair[a,b]:Neg { neg p = Pair (neg (fst p)) (neg (snd p)) };

main = neg (Pair 42 true);