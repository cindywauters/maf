(define needed-function (lambda (x) (display "a")))
(<insert> (define moved-function (lambda (x) (display (+ x 1))(needed-function x)))) ;; uses needed-function with idn 1:9

(define (old-scope x)
 (define needed-function (lambda (x) (display x)))
 (<delete> (define moved-function (lambda (x) (display (+ x 1))(needed-function x)))) ;; uses needed-function with idn 5:10
 (moved-function x))

(old-scope 5)

(define add (lambda (x) (+ x 1)))
(define subtract (lambda (x) (- x 1)))
(define f (lambda (x) x))
(<insert> (define second-moved-function (lambda (x) (display (add x))(display (subtract x))(f x)))) ;; uses adder with idn 11:9

(define (scope-with-nested-lets-reanalysis x)
  (let* ((firstval 5)
         (secondval 10)
         (add (lambda (x) (+ x 3))))
    (+ firstval secondval)
    (<insert> (second-moved-function firstval))   ;; uses same firstval but second-... with idn 14:42
    (let* ((add (lambda (x) (+ x 2)))
           (<delete> (second-moved-function (lambda (x) (display (add x))(display (subtract x))(f x)))) ;; uses adder with idn 22:3
           (end "end"))
      (<delete> (second-moved-function firstval)) ;; uses same firstval but second-... with idn 23:23
      (display end))))

(<insert> (define third-moved-function (lambda (x) (display (add x))(display (subtract x))(f x)))) ;; uses adder with idn 11:9

(define (scope-with-nested-lets-update x)
  (let* ((firstval 5)
         (secondval 10))
    (+ firstval secondval)
    (let* ((<delete> (third-moved-function (lambda (x) (display (add x))(display (subtract x))(f x))))) ;; uses all the same functions as one on line 28 due to let overriding none
        (second-moved-function firstval))))


(scope-with-nested-lets-update 5)