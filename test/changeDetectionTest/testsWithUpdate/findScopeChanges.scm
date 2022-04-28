(define (g x) (dislayln x))

(define (f x)
  (define g (lambda (x) (+ x 1)))
  (g x))


(f 5)