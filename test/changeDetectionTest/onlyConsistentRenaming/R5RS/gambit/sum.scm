;; renamed lambdas/lets: 0
 
(define run (lambda (n)
      ((letrec ((loop (lambda (i sum) (if (< i 0) sum (loop (- i 1) (+ i sum)))))) loop) n 0)))
 
(= (run 10000) 50005000)
 
