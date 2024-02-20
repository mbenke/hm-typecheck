new : a -> Memory[a];

mp = new(pair 1 false);

first_ : Memory[Pair[a,b]] -> Memory[a];
second_ : Memory[Pair[a,b]] -> Memory[b];

class self : HasFirst[a] { first : self -> a; };
class self : HasSecond[a] { second : self -> a; };

instance Pair[a,b] : HasFirst[a];
instance Pair[a,b] : HasSecond[b];

instance Stack[Pair[a,b]] : HasFirst[Stack[a]];
instance Stack[Pair[a,b]] : HasSecond[Stack[b]];

instance Memory[Pair[a,b]] : HasFirst[Memory[a]];
instance Memory[Pair[a,b]] : HasSecond[Memory[b]];

instance Stack[a] : Ref[a];
id x = x;

t1 = {
  let x : Int;
  let p : Pair[Int, Bool];
  p.first = 42;
  p.second = True;
  x = p.first.id;
  x.id = id(p.first);
  let q : Pair[Int,Pair[Int,Bool]];
  q.second.first = 42;
  x = q.second.first;
  }


