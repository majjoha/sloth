I x = x;

john f n = letrec x = I in (f x);

main = letrec f = john f in f f