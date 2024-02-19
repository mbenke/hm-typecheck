id x = x;
ex1 = { id(42) };
instance Stack[a] : Ref[a];
ex2a = { let y : Int; y = id(y) };
// pragma log;
ex2 = {
  let x : Bool ;
  let y : Int;
  x = True;
  y = 42;
  y = id(42);
  let z : Int;
  z = id(y);
};
