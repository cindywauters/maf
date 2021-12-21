;; renamed lambdas/lets: 2
 
(define random-bool (<change>
      (lambda ()
         (= (random 2) 0))
      (lambda ()
         (= (random 2) 0))))
 
(define f (<change>
      (lambda (x)
         (if (random-bool) x (g (cons 'f x))))
      (lambda (_x0)
         (if (random-bool) _x0 (g (cons 'f _x0))))))
 
(define g (lambda (x)
      (if (random-bool) x (f (cons 'g x)))))
 
(f ())
 
