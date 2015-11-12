two succ zero = succ (succ zero);

four succ zero = succ (succ (succ (succ zero)));

add m n succ zero = m succ (n succ zero);

main = add two four