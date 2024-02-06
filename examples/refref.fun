    class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
    };

    instance Stack[Int] : Ref[Int];
    instance Memory[a]: Ref[a] ;

    
    // this does not work - overlapping instances
    // instance (x:Ref[v], r:Ref[x]) => r:Ref[v];

    // Problem: we want (r:Ref[x], x:Ref[v]) => r -> v -> Unit
    // But we get ∀a.(a:Ref[a1], a1:Ref[v]) => a -> b -> Unit
    // (bad generalisation?)
    store2 r v = store(load r) v;

    refOnStack : Stack[Memory[Int]];

    // Fails: no instance of Stack[Memory[Int]]:Ref[a1]
    // toMem = store2 refOnStack 42;

