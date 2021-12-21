;; renamed lambdas/lets: 1
 
(define run (lambda (n)
      ((letrec ((loop (<change>
                       (lambda (i sum)
                          (if (< i 0) sum (loop (- i 1) (+ i sum))))
                       (lambda (_i0 _sum0)
                          (if (< _i0 0)
                             _sum0
                             (loop (- _i0 1) (+ _i0 _sum0)))))))
         loop)
         n
         0)))
 
(= (run 10000) 50005000)
 
