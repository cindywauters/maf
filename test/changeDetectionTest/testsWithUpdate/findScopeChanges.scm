(define g (lambda ()
(<insert>  (define a (lambda () 1)))
(<delete> (define a (lambda () #f)))


(define f (lambda (z) (lambda (w)(lambda ((<update> y x))
    (if (a)
        (displayln (<update> y x))
        (a))))))

(((f 5) 1) 1)))

(g)
