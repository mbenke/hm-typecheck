mutual {
and2 xs = foldr2 and True xs;
foldr2 xs = foldr xs;
// sum2 = foldr2 add 0;
};
// foo = and2 Nil;   