primMloadInt : M[Int] -> Int;
primMstoreInt : M[Int] -> Int -> Unit;

class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance M[Int] : Ref[Int] { load = primMloadInt; store = primMstoreInt };
mi : M[Int];
// main = load mi;
main = store mi 42;