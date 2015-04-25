id x = x;

id2 y = y;

main = let x = (let y = id2 in y) in id 
