(define needed-function (lambda (x) (display "a")))
(<insert> (define moved-function (lambda (x) (display (+ x 1))(needed-function x))))

(define (old-scope2 x)
 (define needed-function (lambda (x) (display x)))
 (<delete> (define moved-function (lambda (x) (display (+ x 1))(needed-function x)))) ;; different needed-function
 (moved-function x))

(old-scope2 5)