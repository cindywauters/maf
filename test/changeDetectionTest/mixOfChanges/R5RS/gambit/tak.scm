;; renamed lambdas/lets: 0
 
(define tak (lambda (x y z)
      (if (not (< y x))
         z
         (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))
 
(define res (= 7 (tak 18 12 6)))
 
res
 
