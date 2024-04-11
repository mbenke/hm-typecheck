type SI;
type MI;
primMloadInt : MI -> Int;
primMstoreInt : MI -> Int -> Unit;
primLoadInt : SI -> Int;
primStoreInt : SI -> Int -> Unit;

class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance MI : Ref[Int] { load = primMloadInt; store = primMstoreInt };
instance SI : Ref[Int] { load = primLoadInt; store = primStoreInt };

mi : MI;
si : SI;
// copy : ∀a b c.(b:Ref[a], c:Ref[a]) => b -> c -> Unit
copy from to = store to (load from);
main = copy si mi;