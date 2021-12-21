;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((id (lambda (x)
                  x))
            (f (lambda (n)
                 (if (<= n 1) 1 (* n (f (- n 1))))))
            (g (lambda (n)
                 (if (<= n 1) 1 (+ (* n n) (g (- n 1)))))))
      (+ ((id f) 3) ((id g) 4)))
   (letrec ((_id0 (lambda (_x0)
                    _x0))
            (_f0 (lambda (_n0)
                   (if (<= _n0 1) 1 (* _n0 (_f0 (- _n0 1))))))
            (_g0 (lambda (_n1)
                   (if (<= _n1 1) 1 (+ (* _n1 _n1) (_g0 (- _n1 1)))))))
      (+ ((_id0 _f0) 3) ((_id0 _g0) 4))))
 
