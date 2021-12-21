;; renamed lambdas/lets: 2
 
(define comprimeer (<change>
      (lambda (metingen)
         (define hulp (lambda (lst prev count)
               (if (null? lst)
                  (list (list prev count))
                  (if (= (car lst) prev)
                     (hulp (cdr lst) prev (+ count 1))
                     (cons (list prev count) (hulp (cdr lst) (car lst) 1))))))
         (if (null? metingen)
            ()
            (hulp (cdr metingen) (car metingen) 1)))
      (lambda (_metingen0)
         (define hulp (lambda (_lst0 _prev0 _count0)
               (if (null? _lst0)
                  (list (list _prev0 _count0))
                  (if (= (car _lst0) _prev0)
                     (hulp (cdr _lst0) _prev0 (+ _count0 1))
                     (cons (list _prev0 _count0) (hulp (cdr _lst0) (car _lst0) 1))))))
         (if (null? _metingen0)
            ()
            (hulp (cdr _metingen0) (car _metingen0) 1)))))
 
(define comprimeer-iter (<change>
      (lambda (metingen)
         (define hulp (lambda (lst prev count res)
               (if (null? lst)
                  (reverse (cons (list prev count) res))
                  (if (= (car lst) prev)
                     (hulp (cdr lst) prev (+ count 1) res)
                     (hulp (cdr lst) (car lst) 1 (cons (list prev count) res))))))
         (if (null? metingen)
            ()
            (hulp (cdr metingen) (car metingen) 1 ())))
      (lambda (_metingen0)
         (define hulp (lambda (_lst0 _prev0 _count0 _res0)
               (if (null? _lst0)
                  (reverse (cons (list _prev0 _count0) _res0))
                  (if (= (car _lst0) _prev0)
                     (hulp (cdr _lst0) _prev0 (+ _count0 1) _res0)
                     (hulp (cdr _lst0) (car _lst0) 1 (cons (list _prev0 _count0) _res0))))))
         (if (null? _metingen0)
            ()
            (hulp (cdr _metingen0) (car _metingen0) 1 ())))))
 
(if (equal? (comprimeer (__toplevel_cons 3.750000e+01 (__toplevel_cons 3.750000e+01 (__toplevel_cons 3.720000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.830000e+01 ())))))))) (__toplevel_cons (__toplevel_cons 3.750000e+01 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3.720000e+01 (__toplevel_cons 1 ())) (__toplevel_cons (__toplevel_cons 3.800000e+01 (__toplevel_cons 3 ())) (__toplevel_cons (__toplevel_cons 3.830000e+01 (__toplevel_cons 1 ())) ())))))
   (equal?
      (comprimeer-iter
         (__toplevel_cons
            3.750000e+01
            (__toplevel_cons
               3.750000e+01
               (__toplevel_cons
                  3.720000e+01
                  (__toplevel_cons
                     3.800000e+01
                     (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.830000e+01 ()))))))))
      (__toplevel_cons
         (__toplevel_cons 3.750000e+01 (__toplevel_cons 2 ()))
         (__toplevel_cons
            (__toplevel_cons 3.720000e+01 (__toplevel_cons 1 ()))
            (__toplevel_cons
               (__toplevel_cons 3.800000e+01 (__toplevel_cons 3 ()))
               (__toplevel_cons (__toplevel_cons 3.830000e+01 (__toplevel_cons 1 ())) ())))))
   #f)
 
