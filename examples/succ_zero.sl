two succ zero = let one = succ zero in succ one;

four succ zero = let three = let two = let one = succ zero in succ one in succ two in succ three;

add m n succ zero = let nsz = n succ zero in m succ nsz;

main = let six = add two four in six