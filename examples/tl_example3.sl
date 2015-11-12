cons a b cc cn = cc a b ; 
hd list = list K abort ; 
tl list = list K1 abort ; 
K x y = x;
K1 x y = y ;
abort = abort ;

hdn n list = if (n = 0) list (hd (hdn n-1 list)); 
infinite x = letrec xs = cons x xs in xs ;
main = hdn 100 (tl (infinite 4))