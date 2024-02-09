    class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
    };

    instance Memory[a]:Ref[a];

    store2 r v = store(load r) v;
    store3 r v = store(load (load r)) v;
    store4 r v = store(load (load (load r))) v;    

    new : a -> Memory[a];
    r1 = new 42;
    r2 = new r1;
    r3 = new(new(new 42));
    u1 = store  r1 1;
    u2 = store2 r2 2;
    u3 = store3 r3 3;