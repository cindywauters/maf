;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((t (lambda (x)
                 (t (+ x 1)))))
      (t 0))
   (letrec ((_t0 (lambda (_x0)
                   (_t0 (+ _x0 1)))))
      (_t0 0)))
 
