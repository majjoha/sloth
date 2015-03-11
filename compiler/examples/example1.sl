I x = x;
K x y = x;
K1 x y = y ;
S f g x = f x (g x) ; 
compose f g x = f (g x) ; 
twice f = compose f f ;

main = let id1 = I I I
in id1 id1 3