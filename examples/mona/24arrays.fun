primMloadInt : Memory[Int] -> Int;
primMstoreInt : Memory[Int] -> Int -> Unit;
primLoad : Stack[a] -> a;
primStore : Stack[a] -> a -> Unit;

class r:Ref[d] { load: r -> d; store: r -> d -> Unit};
instance Memory[Int] : Ref[Int] { load = primMloadInt; store = primMstoreInt };
instance Stack[a] : Ref[a] { load = primLoad; store = primStore };

class a : MemoryBaseType { stride : Itself[a] -> Int };

instance Int : MemoryBaseType { stride x = 1 };

class a:IndexAccessible[baseType] {
   indexAccess : a -> Int -> baseType;
};
primIndexAccess : a -> Int -> b;

class a:IndexAccessible2[index, baseType] {
   indexAccess2 : a -> index -> baseType;
};
primIndexAccess2 : a -> index -> b;

type MemoryArray[a];
instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]] { indexAccess = primIndexAccess };
instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible2[Int, Memory[a]] { indexAccess2 = primIndexAccess };

/*
arrays0 = {
  let array : MemoryArray[Int];
  let x : Int;
  let mx : Memory[Int];
  
  // we probably want a[i] to desugar into one of these
  *array.indexAccess(1) = 42;
  *array.indexAccess(1) = x ;

  // this does not work (LHS is stack, RHS is memory):
  //  x = array.indexAccess(1)
  x = load(*array.indexAccess(1));
  x = load (indexAccess array 1);

  mx = indexAccess array 1;
}
*/

main = {
  let array : MemoryArray[Int];
  let x : Int;
  
  array[1] = 42;
  array[1] = x;

  x = array[1];
}
