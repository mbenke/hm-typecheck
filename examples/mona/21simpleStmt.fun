primMloadInt : M[Int] -> Int;
primMstoreInt : M[Int] -> Int -> Unit;
primLoadInt : Stack[Int] -> Int;
primStoreInt : Stack[Int] -> Int -> Unit;
primLoadBool : Stack[Bool] -> Bool;
primStoreBool : Stack[Bool] -> Bool -> Unit;

class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance M[Int] : Ref[Int] { load = primMloadInt; store = primMstoreInt };
instance Stack[Int] : Ref[Int] { load = primLoadInt; store = primStoreInt };
instance Stack[Bool] : Ref[Bool] { load = primLoadBool; store = primStoreBool };

id x = x;
main0 = { id(42) };
main1 = { let y : Int; y = id(y) };
// pragma log;
main = {
  let x : Bool ;
  let y : Int;
  x = True;
  y = 42;
  y = id(42:Int);
  let z : Int;
  z = id(y);
};
