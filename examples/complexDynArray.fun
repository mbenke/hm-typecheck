/*
    class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
    };
*/
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

    type DynamicArray[a];

    instance Memory[DynamicArray[a]] : IndexAccessible[Memory[a]];
    instance Memory[DynamicArray[a]] : IndexAccessible2[Int, Memory[a]];    

    dynArray : Memory [DynamicArray [Int]];
    x60 = indexAccess dynArray 1;
    x61 = store (indexAccess dynArray 61) 42;
    x62 = store (indexAccess2 dynArray 62) 42;

    pairArray : Memory[DynamicArray[Pair[Int,Bool]]];
    pib = pair 1 false;
    x70 = store (indexAccess pairArray 70) pib;
    x71 = load (indexAccess pairArray 70) ;


    class self : HasFirst[a] { first : self -> a; };
    class self : HasSecond[a] { second : self -> a; };

    instance Pair[a,b] : HasFirst[a];
    instance Pair[a,b] : HasSecond[b];

    instance Memory[Pair[a,b]] : HasFirst[Memory[a]];
    instance Memory[Pair[a,b]] : HasSecond[Memory[b]];

    // pairArray[81].second = false
    x81 = store (second (indexAccess pairArray 81)) false;

    dada : Memory[DynamicArray[Memory[DynamicArray[Int]]]];
    x82 = load (indexAccess dada 82);
    x83 = store (indexAccess x82 83) 17;
    x84 = store (indexAccess (load (indexAccess dada 82)) 83) 42;

    // this should not typecheck
    // bad85 = store (indexAccess (load (indexAccess dada 82)) 83) false;
