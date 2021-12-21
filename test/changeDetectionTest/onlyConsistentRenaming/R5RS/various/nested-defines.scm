;; renamed lambdas/lets: 1
 
(define f (<change>
      (lambda (x)
         (define g (lambda (y)
               (+ x y)))
         (g 5))
      (lambda (_x0)
         (define g (lambda (_y0)
               (+ _x0 _y0)))
         (g 5))))
 
(= (f 0) 5)
 
