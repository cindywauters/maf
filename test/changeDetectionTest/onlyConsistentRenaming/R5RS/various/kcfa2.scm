;; renamed lambdas/lets: 1
 
(<change>
   (let ((res ((lambda (f1) (letrec ((a (f1 #t))) (f1 #f)))
                (lambda (x1)
                   ((lambda (f2)
                      (letrec ((b (f2 #t))
                               (c (f2 #f)))
                         (f2 #t)))
                      (lambda (x2)
                         ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))))
      res)
   (let ((_res0 ((lambda (_f10) (letrec ((_a0 (_f10 #t))) (_f10 #f)))
                  (lambda (_x10)
                     ((lambda (_f20)
                        (letrec ((_b0 (_f20 #t))
                                 (_c0 (_f20 #f)))
                           (_f20 #t)))
                        (lambda (_x20)
                           ((lambda (_z0) (_z0 _x10 _x20)) (lambda (_y10 _y20) _y10))))))))
      _res0))
 
