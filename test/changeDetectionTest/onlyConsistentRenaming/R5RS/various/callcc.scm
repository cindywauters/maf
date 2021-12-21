;; renamed lambdas/lets: 0
 
(define saved ())
 
(define done? #f)
 
(define foo (lambda (x)
      (+ x (call/cc bar))))
 
(define bar (lambda (cnt)
      (set! saved cnt)
      42))
 
(define main (lambda ()
      (let ((res (foo 100)))
         (if done? res (begin (set! done? #t) (saved 3))))))
 
(main)
 
