hej a = a;

g y = y;

f x = x;

funky d = case d of
  1 -> hej;
  2 x xs -> funky xs
  end;

main = let a = g in let b = f in let c = nil in funky a : b : c
