(define (f x) (+ 1 x))

(define (g y)
  (<insert> (display "g"))
  (+ 1 y))

(+ (f 1) (g 1))