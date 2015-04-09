main = head (tail (take 5 (from 6)));

head xs =
   case xs of
   1 -> nil;
   2 x rest -> x
   end;

tail xs =
   case xs of
   1 -> nil;
   2 x rest -> rest
   end;  

from n = n : (from (n+1));

elseBranch xs n = case xs of
                  1 -> pack(1, 0);
                  2 p ps -> p : (take (n-1) ps)
                  end;

strict xs = 
  case xs of
  1 -> pack(1,0);
  2 x rest -> (add x 0) : (strict rest)
  end;

take n xs = if (n=0) pack(1,0) (elseBranch xs n);

nil = pack(1,0);

test = 1 : (add 1 1) : nil