;; renamed lambdas/lets: 1
 
(define f (<change>
      (lambda (x)
         (if (< x 100) f 1))
      (lambda (_x0)
         (if (< _x0 100) f 1))))
 
(define f2 (f 5))
 
(eq? (f2 2) f)
 
