;; renamed lambdas/lets: 0
 
(define result ())
 
(define output (lambda (i)
      (set! result (cons i result))))
 
(define hulp 2)
 
(define haha (lambda (x)
      (let ((hulp (* x hulp)))
         (output hulp))
      (output hulp)
      (set! hulp 4)))
 
(haha 2)
 
(haha 3)
 
(equal? result (__toplevel_cons 4 (__toplevel_cons 12 (__toplevel_cons 2 (__toplevel_cons 4 ())))))
 
