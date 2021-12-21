;; renamed lambdas/lets: 0
 
(define flip (let ((state 0))
      (lambda ()
         (if (= state 0) (set! state 1) (set! state 0))
         state)))
 
(if (= (flip) 1)
   (if (= (flip) 0)
      (if (= (flip) 1) (= (flip) 0) #f)
      #f)
   #f)
 
