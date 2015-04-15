two succ zero = let one = succ zero in succ one;

add m n succ zero = let nsz = n succ zero in m succ nsz;

main = add two two