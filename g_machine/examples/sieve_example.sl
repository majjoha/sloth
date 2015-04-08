main = (take 5 (sieve (from 2)));

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

tailn xs n =
  case xs of
  1 -> nil;
  2 x rest -> if (n = 1) rest (tailn rest (n-1))
  end;

strict xs = 
  case xs of
  1 -> pack(1,0);
  2 x rest -> (add x 0) : (strict rest)
  end;

from n = n : (from (n+1));

sieve xs = case xs of
           1 -> pack(1,0);
           2 p ps -> p : (sieve (filter (nonMultiple p) ps))
           end;

filter predicate xs = case xs of
                      1 -> pack(1, 0);
                      2 p ps -> let rest = (filter predicate ps) in
                      (if (predicate p) (p : rest) rest)
                      end;

nonMultiple p n = ((n/p)*p) != n;

elseBranch xs n = case xs of
                  1 -> pack(1, 0);
                  2 p ps -> p : (take (n-1) ps)
                  end;

take n xs = if (n=0) pack(1,0) (elseBranch xs n)
