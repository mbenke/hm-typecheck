// class Ref is now primitive
/*
class ref : Ref[deref] {
      load : ref -> deref;
      store : ref -> deref -> Unit;
};
*/
    

store2 r v = store(load r) v;


