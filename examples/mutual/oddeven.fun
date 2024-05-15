mutual {
   receven n = ifzero n true (recodd (sub n 1));
   recodd n = let m = (sub n 1) in ifzero m true (receven m);
}