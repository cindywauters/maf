;; renamed lambdas/lets: 2
 
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (<change>
                   (lambda (f x)
                      (f (n f x)))
                   (lambda (_f0 _x0)
                      (_f0 (n _f0 _x0))))))
         (plus (lambda (m n)
                 (lambda (f x)
                    (m f (n f x))))))
   ((plus (inc (inc (inc zero))) (plus (inc (inc zero)) (inc zero)))
      (<change>
         (lambda (x)
            (+ x 1))
         (lambda (_x0)
            (+ _x0 1)))
      0))
 
