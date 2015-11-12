main = (take 10 (sieve (from 2)));

from n = n : (from (n+1));

sieve ls =
  case ls of
  1 -> nil;
  2 x xs -> x : (sieve (filter (nonMultiple x) xs))
  end;

filter predicate ls =
  case ls of
  1 -> nil;
  2 x xs -> let rest = (filter predicate xs) in
            (if (predicate x) (x : rest) rest)
  end;

nonMultiple p n = ((n/p)*p) != n;

take n ls = 
  case ls of
  1 -> nil;
  2 x xs -> if (n=0) (nil) (x : (take (n-1) xs))
  end
