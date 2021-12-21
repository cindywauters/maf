;; renamed lambdas/lets: 1
 
(define do-something (lambda ()
      10))
 
(define id (lambda (y)
      (do-something)
      y))
 
(define r1 ((id (lambda (a) a)) #t))
 
(define r2 ((id (<change> (lambda (b) b) (lambda (_b0) _b0))) #f))
 
r1
 
