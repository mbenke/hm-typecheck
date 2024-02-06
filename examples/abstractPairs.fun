class ref : Ref[deref] {
  load : ref -> deref;
  store : ref -> deref -> Unit;
};

instance Memory[a]: Ref[a] ;

first : Memory[Pair[a,b]] -> Memory[b];
new : a -> Memory[a];

mp = new(pair 1 false);

first_ : Memory[Pair[a,b]] -> Memory[a];
second_ : Memory[Pair[a,b]] -> Memory[b];

class self : HasFirst[a] { first : self -> a; };
class self : HasSecond[a] { second : self -> a; };

instance Pair[a,b] : HasFirst[a];
instance Pair[a,b] : HasSecond[b];

instance Memory[Pair[a,b]] : HasFirst[Memory[a]];
instance Memory[Pair[a,b]] : HasSecond[Memory[b]];

v1 = second mp;
v2 = load v1;
v3 = store (first mp) 2;