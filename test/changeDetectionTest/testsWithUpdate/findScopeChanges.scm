;(define something (lambda ()
 (<insert> (define rec-fun (lambda (x)
    (if (< x 0)
         x
         (rec-fun (- x 1))))))

(define (g y)
    (letrec ((<delete> (rec-fun (lambda (x) (if (< x 0) x (rec-fun (- x 1)))))))
     (rec-fun y)))

(g 5);))

;(something)
