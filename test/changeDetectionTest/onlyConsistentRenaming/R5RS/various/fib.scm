;; renamed lambdas/lets: 1
 
(define fib (<change>
      (lambda (n)
         (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
      (lambda (_n0)
         (if (< _n0 2)
            _n0
            (+ (fib (- _n0 1)) (fib (- _n0 2)))))))
 
(fib 10)
 
