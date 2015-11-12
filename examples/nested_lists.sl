repeat n item =
  if (n = 0) (nil) (item : (repeat (n-1) item));

main = let i = (2 : 2) in repeat 17 i