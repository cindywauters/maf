(<delete> (define g (lambda (x) (+ x 1))))

(define (f x)
    (let (((<update> z f) (lambda (y) (+ y x))))
        (g ((<update> z f) x))))

(f 5)



