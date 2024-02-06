class ref : Ref[deref] {
  load : ref -> deref;
  store : ref -> deref -> Unit;
};

instance Memory[a]: Ref[a] ;

first : Memory[Pair[a,b]] -> Memory[b];
new : a -> Memory[a];

mp = new(pair 1 false);

first : Memory[Pair[a,b]] -> Memory[a];
second : Memory[Pair[a,b]] -> Memory[b];

v1 = second mp;
v2 = load v1;
v3 = store (first mp) 2;