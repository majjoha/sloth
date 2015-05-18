map f xs = case xs of
                1      -> nil;
                2 x xr -> (f x) : (map f xr)
                end;

add1 n = n+1;

nats = 0 : map add1 nats;

take n ls =
	case ls of
	1 -> nil;
	2 x rest -> if (n=0) (nil) (x : (take (n-1) rest))
	end
;

main = take 500 nats