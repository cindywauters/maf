;; renamed lambdas/lets: 3
 
(let ((res ((lambda (f1) (letrec ((a (f1 #t))) (f1 #f)))
             (lambda (x1)
                ((lambda (f2)
                   (<change>
                      (letrec ((b (f2 #t)))
                         (f2 #f))
                      (letrec ((_b0 (f2 #t)))
                         (f2 #f))))
                   (lambda (x2)
                      ((<change>
                         (lambda (f3)
                            (letrec ((c (f3 #t)))
                               (f3 #f)))
                         (lambda (_f30)
                            (letrec ((_c0 (_f30 #t)))
                               (_f30 #f))))
                         (<change>
                            (lambda (x3)
                               ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1)))
                            (lambda (_x30)
                               ((lambda (_z0) (_z0 x1 x2 _x30)) (lambda (_y10 _y20 _y30) _y10)))))))))))
   res)
 
