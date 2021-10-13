;(define (test x)
; (let ((y (<change> #f 1)))
;    (+ x (<change> 1 y))))

(define (test x)
 (<change> (+ x 1)
           (let ((y 1))(+ x y))))

(test 5)