length xs = case xs of
            1 -> 0;
            2 y ys -> (1 + (length ys))
            end;

main = length (1 : 1 : 1 : pack(1, 0))

