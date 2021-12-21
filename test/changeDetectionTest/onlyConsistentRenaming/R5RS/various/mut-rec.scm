;; renamed lambdas/lets: 2
 
(letrec ((even? (<change>
                  (lambda (x)
                     (if (= x 0) #t (odd? (- x 1))))
                  (lambda (_x0)
                     (if (= _x0 0) #t (odd? (- _x0 1))))))
         (odd? (<change>
                 (lambda (x)
                    (if (= x 0) #f (even? (- x 1))))
                 (lambda (_x0)
                    (if (= _x0 0) #f (even? (- _x0 1)))))))
   (even? 4))
 
