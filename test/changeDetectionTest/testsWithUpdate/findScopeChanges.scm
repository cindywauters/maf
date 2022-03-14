(define (g y)
  (let ((<delete> (f (lambda (x) (display x)))))
    (let* ((<insert> (f (lambda (x) (display x)))))
        (f y))))

(g 5)