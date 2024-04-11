primMloadInt : Memory[Int] -> Int;
primMstoreInt : Memory[Int] -> Int -> Unit;
primLoad : Stack[a] -> a;
primStore : Stack[a] -> a -> Unit;

class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance Memory[Int] : Ref[Int] { load = primMloadInt; store = primMstoreInt };
instance Stack[a] : Ref[a] { load = primLoad; store = primStore };
// store : ∀a b.(a:Ref[b]) => a -> b -> Unit
copy from to = store to (load from);
// copy : ∀a b.(b:Ref[d], a:Ref[d]) => a -> b -> Unit

main = {
  let x : Memory[Int];
  let y : Memory[Int];
  *x = 42;
  x = y;
  // *x = y; // does not work, but the following work
  *x = load y;
  *x = *y;
  copy x y;

}
