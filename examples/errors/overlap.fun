class a : C { f : a -> Int };

instance Pair[Int,a] : C { f = fst };
instance Pair[a,Int] : C { f = snd };
