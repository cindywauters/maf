;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((h (lambda (b)
                 (letrec ((g (lambda (z)
                               z))
                          (f (lambda (k)
                               (if b (k 1) (k 2))))
                          (y (f (lambda (x) x))))
                    (g y))))
            (x (h #t))
            (y (h #f)))
      y)
   (letrec ((_h0 (lambda (_b0)
                   (letrec ((_g0 (lambda (_z0)
                                   _z0))
                            (_f0 (lambda (_k0)
                                   (if _b0 (_k0 1) (_k0 2))))
                            (_y1 (_f0 (lambda (_x1) _x1))))
                      (_g0 _y1))))
            (_x0 (_h0 #t))
            (_y0 (_h0 #f)))
      _y0))
 
