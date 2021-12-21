;; renamed lambdas/lets: 0
 
(define my-cons (lambda (el lst)
      (cons el lst)))
 
(define my-list (my-cons 1 (my-cons 2 (my-cons 3 ()))))
 
my-list
 
