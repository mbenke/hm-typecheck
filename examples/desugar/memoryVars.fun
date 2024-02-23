instance Stack[a] : Ref[a];
instance Memory[a] : Ref[a];

m0 = {
  let x : Memory[Int];
  let y : Memory[Int];
  *x = 42;
  x = y;
  // *x = y; // does not work, but the following work
  *x = y.load;
  *x = *y;
}
