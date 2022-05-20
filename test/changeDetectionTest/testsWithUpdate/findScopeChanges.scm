( define ((<update> f fib) n )
 (if (< n 2)
 n
 (+ ((<update> f fib) (- n 1) )
 ((<update> f fib) (- n 2) ) ) ) )

 ((<update> f fib) 5)

