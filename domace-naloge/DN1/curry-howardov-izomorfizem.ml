let rec phi1 (a, b) = (b, a)

let psi1 = phi1





let phi3 (a, (b, c)) = ((a, b), c)

let psi3 ((a, b), c) = (a, (b, c))





let phi7 c (a, b) = (fst c (a, b), snd c (a, b))