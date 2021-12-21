;; renamed lambdas/lets: 1
 
(let ((sq (<change>
            (lambda (x)
               (* x x))
            (lambda (_x0)
               (* _x0 _x0)))))
   (sq 2)
   (sq 3))
 
