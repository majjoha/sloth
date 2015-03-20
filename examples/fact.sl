fact n = if (eq n 0) 1 (mul (fact (sub 1 n)) n);
main = fact 5
