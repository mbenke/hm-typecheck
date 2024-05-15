mutual {
and2 xs = foldr2 and True xs;
foldr2 xs = foldr xs;
// error: foldr2 is monomorphic
sum2 = foldr2 add 0;
};
