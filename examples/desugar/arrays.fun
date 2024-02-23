instance Memory[a] : Ref[a];
instance Stack[a] : Ref[a];

instance Int : MemoryBaseType;

class a:IndexAccessible[baseType] {
   indexAccess : a -> Int -> baseType;
};

class a:IndexAccessible2[index, baseType] {
   indexAccess2 : a -> index -> baseType;
};

type MemoryArray[a];
instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]];
instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible2[Int, Memory[a]];

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

arrays = {
  let array : MemoryArray[Int];
  let x : Int;
  
  array[1] = 42;
  array[1] = x;

  x = array[1];
}
