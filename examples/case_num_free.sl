hej a = a;

g y = y;

f x = x;

d = g : f : nil;

funky thing = case thing of
  1 -> hej;
  2 x xs -> funky xs
  end;

main = funky d
