condition = eq 1 1;
funky n = mul (sub n 1) 5;
main = if condition (add 42 2) (funky 2)
