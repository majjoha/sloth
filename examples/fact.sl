fact n = if (eq n 0) 1 (mul (fact (sub n 1)) n);
main = fact 2
