    class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
    };

    

    // Problem: we want (r:Ref[x], x:Ref[v]) => r -> v -> Unit
    // But we get ∀a.(a:Ref[a1], a1:Ref[v]) => a -> b -> Unit
    // (bad generalisation?)
    store2 r v = store(load r) v;


