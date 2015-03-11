cons a b cc cn = cc a b ; 
hd list = list K abort ; 
tl list = list K1 abort ; 
K x y = x;
K1 x y = y ;
abort = abort ;

infinite x = letrec xs = cons x xs in xs ;
main = hd (infinite 4)