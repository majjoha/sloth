id x = x;

john f = letrec x = id in (f x);

main = letrec f = john in f f
