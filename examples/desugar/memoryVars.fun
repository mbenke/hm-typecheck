instance Stack[a] : Ref[a];
instance Memory[a] : Ref[a];

// store : ∀a b.(a:Ref[b]) => a -> b -> Unit

m0 = {
  let x : Memory[Int];
  let y : Memory[Int];
  *x = 42;
  x = y;
  // *x = y; // does not work, but the following work
  *x = load y;
  *x = *y;
}
