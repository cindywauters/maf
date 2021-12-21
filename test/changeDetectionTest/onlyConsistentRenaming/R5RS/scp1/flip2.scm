;; renamed lambdas/lets: 1
 
(define make-flip (<change>
      (lambda ()
         (let ((state 0))
            (lambda ()
               (if (= state 0) (set! state 1) (set! state 0))
               state)))
      (lambda ()
         (let ((_state0 0))
            (lambda ()
               (if (= _state0 0)
                  (set! _state0 1)
                  (set! _state0 0))
               _state0)))))
 
(define flip (make-flip))
 
(if (= (flip) 1)
   (if (= (flip) 0)
      (if (= (flip) 1) (= (flip) 0) #f)
      #f)
   #f)
 
