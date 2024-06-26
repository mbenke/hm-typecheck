type Color = R | G | B;

fromEnum : Color -> Int;
fromEnum c = case c of {
  R -> 4;
  G -> 2;
  B -> 42;
};
main = fromEnum B;