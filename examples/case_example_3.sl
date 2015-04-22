a x = x;

b y = y;

c = nil;

p = a : b : c;

main = case p of
  1 -> b;
  2 x xs -> x
  end
