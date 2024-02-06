    class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
    };

    instance Memory[a]: Ref[a] ;

    type Itself[a] = Proxy ;
    class a:MemoryBaseType {
      stride : Itself[a] -> Int;
    };

    instance Int : MemoryBaseType;
    class a:IndexAccessible[baseType] {
       indexAccess : a -> Int -> baseType;
    };
    // function indexAccess(array:a, index:uint256) -> baseType;
    // indexAccess : (a:IndexAccessible[baseType]) => a -> Int -> baseType;
    // can this return a reference?
    type MemoryArray[a];
    instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]];

    array : MemoryArray[Int];
    x44 = store (indexAccess array 1) 42; // FIXME: context reduction
