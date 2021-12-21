;; renamed lambdas/lets: 1
 
(letrec ((lp1 (<change>
                (lambda (i x)
                   (letrec ((a (= 0 i)))
                      (if a
                         x
                         (letrec ((lp2 (lambda (j f y)
                                         (letrec ((b (= 0 j)))
                                            (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))))
                            (lp2 10 (lambda (n) (+ n i)) x)))))
                (lambda (_i0 _x0)
                   (letrec ((_a0 (= 0 _i0)))
                      (if _a0
                         _x0
                         (letrec ((_lp20 (lambda (_j0 _f0 _y0)
                                           (letrec ((_b0 (= 0 _j0)))
                                              (if _b0
                                                 (lp1 (- _i0 1) _y0)
                                                 (_lp20 (- _j0 1) _f0 (_f0 _y0)))))))
                            (_lp20 10 (lambda (_n0) (+ _n0 _i0)) _x0))))))))
   (lp1 10 0))
 
