type Pair[a,b] = Pair[a,b];

primMap : (a -> b) -> List[a] ->List[b];
primNegInt : Int -> Int;

map = primMap;
singleton x = Cons x Nil;

class a:Neg { neg : a -> a};
instance Bool:Neg { neg = not };
instance Int:Neg { neg = primNegInt };
instance (a:Neg) => List[a]:Neg { neg = map (neg:a->a) };

// instance (a:Neg) => List[a]:Neg { neg = map neg };

main = neg (singleton(singleton 42));
