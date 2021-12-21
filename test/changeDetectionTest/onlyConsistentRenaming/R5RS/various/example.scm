;; renamed lambdas/lets: 1
 
(define map (<change>
      (lambda (f lst)
         (if (null? lst)
            ()
            (cons (f (car lst)) (map f (cdr lst)))))
      (lambda (_f0 _lst0)
         (if (null? _lst0)
            ()
            (cons (_f0 (car _lst0)) (map _f0 (cdr _lst0)))))))
 
(define inc (lambda (n)
      (+ n 1)))
 
(map inc (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
 
