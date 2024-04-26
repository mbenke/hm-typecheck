class a : C[b] { f : b -> b; };

instance Int : C[Int] { f x = 15 };
instance Bool : C[Int] { f x = 42 };

main = f 0