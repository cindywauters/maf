;; renamed lambdas/lets: 1
 
(define double (lambda (x)
      (+ x x)))
 
(define halve (lambda (x)
      (/ x 2)))
 
(define sim-multiply (<change>
      (lambda (a b)
         (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1)))))
      (lambda (_a0 _b0)
         (if (zero? _b0)
            1
            (+ 1 (sim-multiply _a0 (- _b0 1)))))))
 
(define sim-fast-multiply (lambda (a b)
      (if (zero? b)
         1
         (if (even? b)
            (+ 1 (sim-fast-multiply (double a) (halve b)))
            (+ 1 (sim-fast-multiply a (- b 1)))))))
 
(if (= (sim-multiply 14 2365) 2366)
   (= (sim-fast-multiply 14 2365) 19)
   #f)
 
