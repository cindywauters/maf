;; renamed lambdas/lets: 2
 
(define result ())
 
(define output (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define count1 (<change>
      (lambda (x)
         (if (= 0 x)
            (display x)
            (begin
               (display x)
               (count1 (- x 1)))))
      (lambda (_x0)
         (if (= 0 _x0)
            (display _x0)
            (begin
               (display _x0)
               (count1 (- _x0 1)))))))
 
(define count2 (lambda (x)
      (if (= 0 x)
         (display x)
         (begin
            (count2 (- x 1))
            (display x)))))
 
(count1 4)
 
(count2 4)
 
(equal?
   result
   (__toplevel_cons
      4
      (__toplevel_cons
         3
         (__toplevel_cons
            2
            (__toplevel_cons
               1
               (__toplevel_cons
                  0
                  (__toplevel_cons
                     0
                     (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ())))))))))))
 
