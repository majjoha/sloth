I x = x;
K x y = x;
K1 x y = y ;
S f g x = f x (g x) ; 
compose f g x = f (g x) ; 
twice f = compose f f ;
cons a b cc cn = cc a b ; 
nil cc cn = cn ;
hd list = list K abort ; 
tl list = list K1 abort ; 
abort = abort ;

infinite x = letrec xs = cons x xs in xs ;
main = hd (tl (tl (infinite 4)))