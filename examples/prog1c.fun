class a : Eq { eq : a -> a -> Bool };

instance Int : Eq;
instance Bool : Eq;
instance a : Eq => List[a] : Eq;
instance (a:Eq, b:Eq) => Pair[a,b] : Eq;

type Option[a] = None | Some[a];
pure = Some;
instance a:Eq => Option[a] : Eq;

idInt1 x = x:Int;
idInt2 (x:Int) = x;

// len = foldr (\ c n -> add 1 n) 0;
len xs = case xs of { Nil -> 0; Cons y ys -> add 1 (len ys) };
sum = foldr add 0;
// elem x xs = foldr (\y r -> or (eq x y) r) false xs;
elem x xs = case xs of {
  Nil -> False;
  Cons y ys -> or (eq x y) (elem  x ys) };
  
f1 = elem 1 nil;
f2 x xs = or (eq x (head xs)) (eq (tail xs) nil);
f2Int (x:Int) xs = or (eq x (head xs)) (eq (tail xs) nil); //f2 x xs;
f3 x xs = or (elem x xs) (eq (tail xs) nil);
pair12 = pair 1 2;
f4 = elem pair12 nil;
f5 x  = elem (pair 1 x) nil;
f6 x ys = elem (pair 1 x) ys;
f7 = f6 false nil;

