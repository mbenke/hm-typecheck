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
instance Memory[a] : Ref[a];
id x = x;


m1 = {
  let x : Memory[Int];
  let y : Memory[Int];  
  *x = 42;
  x = y;

  let p : Pair[Int, Bool];
  let mp : Memory[Pair[Int, Bool]];

  p.first = 1;
  p.second = False;
  p = pair 1 False;
  *mp = p;
  *mp.first = 42;


  *x = mp.load.first;
  // *x = mp.first does not work, but we may write this as *x = mp->first

  let mq : Memory[Pair[Int, Pair[Int,Bool]]];
  // this is a problem - mq.second has wrong type
  // *mq.second = mp

  // these seem to work though
  *mq.second = mp.load;
  *mq.second = p;
  *x = mq.load.second.first;
}

