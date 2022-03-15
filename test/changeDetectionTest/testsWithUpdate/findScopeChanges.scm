(<delete> (define f (lambda (x) (display x))))

(define (g y)
    (let* ((<insert> (f (lambda (x) (display x)))))
        (f y)))

(g 5)



