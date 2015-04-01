casejumper p = case p of
  1 -> 7;
  2 x xs -> x
  end;

main = casejumper (42 : 2 : 5)
