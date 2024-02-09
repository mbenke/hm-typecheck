class ref : Ref[deref] {
  load : ref -> deref;
  store : ref -> deref -> Unit;
};

instance Memory[a]:Ref[a];

first : Memory[Pair[a,b]] -> Memory[a];
second : Memory[Pair[a,b]] -> Memory[b];

new : a -> Memory[a];

storefst r v = store (first r) v;
loadfst r = load (first r) ;
store2 r v = storefst (loadfst r) v;
store3 r v = storefst(loadfst(loadfst r)) v;

r1 = new (pair 42 false);
r2 = new (pair r1 true);
r3 = new (pair r2 17);


u1 = store  (first r1) 1;
u2 = store2 r2 2;
u3 = store3 r3 3;
