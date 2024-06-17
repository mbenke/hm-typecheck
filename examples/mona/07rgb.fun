type Color = R | G | B;

fromEnum : Color -> Int;
fromEnum c = case c of {
  B -> 4;
};
main = fromEnum B;