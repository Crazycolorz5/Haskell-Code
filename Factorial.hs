factorial::(Integral a)=>a->a
factorial n = factorial_rec n id

factorial_rec::(Integral a)=>a->(a->a)->a
factorial_rec 0 cont = cont 1
factorial_rec n cont = factorial_rec (pred n) (\acc->cont(n*acc))

main = getLine >>= print . factorial . read
