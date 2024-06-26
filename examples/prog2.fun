type Stack[a];
siExample : Stack[Int];

instance Stack[Int] : Ref[Int];
instance SI : Ref[Int];
instance Memory[a]: Ref[a] ;
pragma nocoverage;
instance (ra:Ref[a], rb:Ref[b]) => Pair[ra,rb] : Ref[Pair[a,b]];
mi = newMRef 42;
x1 = load mi;
x2 = load siExample;
f3 x = load (newMRef (add x 1));
x4 = pair mi siExample;
x5 = load x4;

unit = Unit;

// store : (ref: Ref[a]) => ref -> a -> Unit;
x6 = store mi x2;

pragma log;
copy from to = store to (load from);
pragma nolog;
x7 = copy siExample mi;
