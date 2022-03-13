;(define add (lambda (x) (+ x 1)))
;(define subtract (lambda (x) (- x 1)))
;(define f (lambda (x) x))

(<insert> (define third-moved-function (lambda (x) (display x)))) ;; uses adder with idn 11:9

(define (scope-with-nested-lets-update x)
 ; (let* ((firstval 5)
  ;       (secondval 10))
   ; (+ firstval secondval)
    (let* ((<delete> (third-moved-function (lambda (x) (display x))))) ;; uses all the same functions as one on line 28 due to let overriding none
        (second-moved-function x)))

(scope-with-nested-lets-update 5)