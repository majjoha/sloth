take n ls =
	case ls of
	1 -> nil;
	2 x rest -> if (n=0) (nil) (x : (take (n-1) rest))
	end
;

main = letrec ones = 1 : ones in take 100000 ones