repeat n item =
  if (n = 0) (nil) (item : (repeat (n-1) item));

take n ls =
  case ls of
  1 -> nil;
  2 x rest -> if (n=0) (nil) (x : (take (n-1) rest))
  end
;

main = let i = (2 : nil) in repeat 17 i