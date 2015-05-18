from n = n : (from (n+1)); 

take n ls =
	case ls of
	1 -> nil;
	2 x rest -> if (n=0) (nil) (x : (take (n-1) rest))
	end
;

main = take 10 (from 1)