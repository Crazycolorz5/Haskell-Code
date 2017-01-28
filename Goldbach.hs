import MyUsefulFunctions

--returns a tuple of primes that adds up to x
goldbach x = goldbach_helper primes x

goldbach_helper primes x = if isPrime (x - head primes) then (head primes, x - head primes) else goldbach_helper (tail primes) x