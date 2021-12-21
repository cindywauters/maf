;; renamed lambdas/lets: 1
 
(letrec ((zero (<change>
                 (lambda (f x)
                    x)
                 (lambda (_f0 _x0)
                    _x0)))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x))))))
   ((inc (inc zero)) (lambda (x) (+ x 1)) 0))
 
