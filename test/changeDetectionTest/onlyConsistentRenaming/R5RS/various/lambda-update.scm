;; renamed lambdas/lets: 1
 
(define the-lambda ())
 
(define update-lambda! (<change>
      (lambda (n)
         (set! the-lambda (lambda ()
                          n)))
      (lambda (_n0)
         (set! the-lambda (lambda ()
                          _n0)))))
 
(update-lambda! 0)
 
(update-lambda! 1)
 
(define res (the-lambda))
 
res
 
