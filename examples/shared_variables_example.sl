from n = n : (from (n+1)); 

take n ls =
  case ls of
  1 -> nil;
  2 x rest -> if (n=0) (nil) (x : (take (n-1) rest))
  end
;

repeat n item =
  if (n = 0) (nil) (item : (repeat (n-1) item));

main = let expensivecomputation = take 10 (from 1) in 
       repeat 1 expensivecomputation