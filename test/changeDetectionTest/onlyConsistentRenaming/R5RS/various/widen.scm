;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((g (lambda ()
                 1))
            (f (lambda (n)
                 (if (= n 0) 0 (+ (f (- n 1)) (g))))))
      (f 10))
   (letrec ((_g0 (lambda ()
                   1))
            (_f0 (lambda (_n0)
                   (if (= _n0 0) 0 (+ (_f0 (- _n0 1)) (_g0))))))
      (_f0 10)))
 
