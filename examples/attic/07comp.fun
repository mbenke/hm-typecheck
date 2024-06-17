id x = x;
comp f g x = f(g x);
main = comp id id 42;