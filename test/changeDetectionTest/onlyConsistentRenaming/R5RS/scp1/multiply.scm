;; renamed lambdas/lets: 0
 
(define rec-multiply (lambda (a b)
      (if (zero? b) 0 (+ a (rec-multiply a (- b 1))))))
 
(define iter-multiply (lambda (a b)
      (define iter (lambda (result counter)
            (if (zero? counter)
               result
               (iter (+ result a) (- counter 1)))))
      (iter 0 b)))
 
(= 10 (rec-multiply 5 2) (iter-multiply 5 2))
 
