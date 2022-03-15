(<insert> (define f (lambda (x) (display x))))

(define (g y)
    (let* ((<delete> (f (lambda (x) (display x)))))
        (f y)))

(g 5)



