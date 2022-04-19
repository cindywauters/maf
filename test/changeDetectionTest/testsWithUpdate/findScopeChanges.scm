(define (f x) x)

(define g
    (lambda (a)
    (+ a (<change> 1 2))))

((f g) 5)

