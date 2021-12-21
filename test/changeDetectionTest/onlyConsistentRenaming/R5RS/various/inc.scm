;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((inc (lambda (x)
                   (+ x 1))))
      (inc (inc 2)))
   (letrec ((_inc0 (lambda (_x0)
                     (+ _x0 1))))
      (_inc0 (_inc0 2))))
 
