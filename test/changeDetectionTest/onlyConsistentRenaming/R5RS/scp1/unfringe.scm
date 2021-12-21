;; renamed lambdas/lets: 2
 
(define unfringe-1 (lambda (l)
      (if (null? l)
         ()
         (if (null? (cdr l))
            (list (car l))
            (list (car l) (unfringe-1 (cdr l)))))))
 
(define unfringe-2 (lambda (l)
      (define pair (<change>
            (lambda (l)
               (if (null? l)
                  ()
                  (if (null? (cdr l))
                     (list l)
                     (cons (list (car l) (cadr l)) (pair (cddr l))))))
            (lambda (_l0)
               (if (null? _l0)
                  ()
                  (if (null? (cdr _l0))
                     (list _l0)
                     (cons (list (car _l0) (cadr _l0)) (pair (cddr _l0))))))))
      ((<change>
         (letrec ((loop (lambda (l)
                          (if (let ((__or_res (null? l))) (if __or_res __or_res (null? (cdr l))))
                             l
                             (loop (pair l))))))
            loop)
         (letrec ((_loop0 (lambda (_l0)
                            (if (let ((___or_res0 (null? _l0))) (if ___or_res0 ___or_res0 (null? (cdr _l0))))
                               _l0
                               (_loop0 (pair _l0))))))
            _loop0))
         l)))
 
(if (equal? (unfringe-1 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ())))))))))) (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons (__toplevel_cons 8 (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) ())) ())) ())) ())))
   (equal?
      (unfringe-2
         (__toplevel_cons
            1
            (__toplevel_cons
               2
               (__toplevel_cons
                  3
                  (__toplevel_cons
                     4
                     (__toplevel_cons
                        5
                        (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 2 ()))
                  (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 5 (__toplevel_cons 6 ()))
                     (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                  ()))
            (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 9 ()) ()) ()) ()))
         ()))
   #f)
 
