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
    
    class a:IndexAccessible2[index, baseType] {
       indexAccess2 : a -> index -> baseType;
    };

    type MemoryArray[a];
    instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible[Memory[a]];
    instance (a:MemoryBaseType) => MemoryArray[a]:IndexAccessible2[Int, Memory[a]];

    array : MemoryArray[Int];
    x51 = store (indexAccess array 1) 42;
    x52 = store (indexAccess2 array 1) 42;

    x53 = indexAccess array 1;
    x54 = indexAccess array 1;

    type DynamicArray[a];

    instance Memory[DynamicArray[a]] : IndexAccessible[Memory[a]];
    instance Memory[DynamicArray[a]] : IndexAccessible2[Int, Memory[a]];    

    dynArray : Memory [DynamicArray [Int]];
    x60 = indexAccess dynArray 1;
    x61 = store (indexAccess dynArray 1) 42;
    x62 = store (indexAccess2 dynArray 1) 42
