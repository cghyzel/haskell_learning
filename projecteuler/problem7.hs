import qualified FunStuff.PrimeNumbers as Primes
-- I got an off by one error with 10001 because the index starts at 0 not 1
solution:: Integer
solution = Primes.primeNumbers !! 10000