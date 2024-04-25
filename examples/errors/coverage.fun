class a : C[b] {
    f : a -> b -> unit;
};

type A;
type B;
instance A:C[a];

x:A;
y:B;

t = f x x;
t = f x y;

