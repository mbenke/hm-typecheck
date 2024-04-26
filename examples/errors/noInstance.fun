class ref : Ref[deref] {
  load : ref -> deref;
  store : ref -> deref -> Unit;
};

instance Memory[a]: Ref[a] ;

mi : Memory[Int];
mb : Memory[Bool];

id x = x;

// x1 = load id;
/*
Error: 
no instance of t -> t:Ref[u]
  - in x1
*/

x2 = add 1 (load id)