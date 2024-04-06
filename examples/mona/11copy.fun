primMloadInt : M[Int] -> Int;
primMstoreInt : M[Int] -> Int -> Unit;
primLoadInt : Stack[Int] -> Int;
primStoreInt : Stack[Int] -> Int -> Unit;
class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance M[Int] : Ref[Int] { load = primMloadInt; store = primMstoreInt };
instance Stack[Int] : Ref[Int] { load = primLoadInt; store = primStoreInt };

mi : M[Int];
si : Stack[Int];
// copy : ∀a b c.(b:Ref[a], c:Ref[a]) => b -> c -> Unit
copy from to = store to (load from);
main = copy si mi;