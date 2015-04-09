fact n = if (n = 0) 1 ((fact (n-1)) * n);

main = factn 5;

factn n = if (n = 0) (fact 0) ((fact n) : (factn (n-1)))
