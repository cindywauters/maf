;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((id (lambda (x)
                  x))
            (blur (lambda (y)
                    y))
            (lp (lambda (a n)
                  (if (<= n 1)
                     (id a)
                     (letrec ((r ((blur id) #t))
                              (s ((blur id) #f)))
                        (not ((blur lp) s (- n 1)))))))
            (res (lp #f 2)))
      res)
   (letrec ((_id0 (lambda (_x0)
                    _x0))
            (_blur0 (lambda (_y0)
                      _y0))
            (_lp0 (lambda (_a0 _n0)
                    (if (<= _n0 1)
                       (_id0 _a0)
                       (letrec ((_r0 ((_blur0 _id0) #t))
                                (_s0 ((_blur0 _id0) #f)))
                          (not ((_blur0 _lp0) _s0 (- _n0 1)))))))
            (_res0 (_lp0 #f 2)))
      _res0))
 
