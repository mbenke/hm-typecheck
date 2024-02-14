pragma log;
store2 r v = store(load r) v;
pragma nolog;

foo x = Unit;
