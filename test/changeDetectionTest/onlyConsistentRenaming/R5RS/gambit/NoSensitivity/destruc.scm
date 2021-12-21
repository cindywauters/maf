;; renamed lambdas/lets: 2
 
(define append-to-tail! (lambda (x y)
      (if (null? x)
         y
         ((<change>
            (letrec ((loop (lambda (a b)
                             (if (null? b)
                                (begin
                                   (set-cdr! a y)
                                   x)
                                (loop b (cdr b))))))
               loop)
            (letrec ((_loop0 (lambda (_a0 _b0)
                               (if (null? _b0)
                                  (begin
                                     (set-cdr! _a0 y)
                                     x)
                                  (_loop0 _b0 (cdr _b0))))))
               _loop0))
            x
            (cdr x)))))
 
(define destructive (<change>
      (lambda (n m)
         (let ((l (letrec ((__do_loop (lambda (i a)
                                       (if (= i 0) a (__do_loop (- i 1) (cons () a))))))
                    (__do_loop 10 ()))))
            (letrec ((__do_loop (lambda (i)
                                  (if (= i 0)
                                     l
                                     (begin
                                        (if (null? (car l))
                                           (letrec ((__do_loop (lambda (l)
                                                                 (if (null? l)
                                                                    #f
                                                                    (begin
                                                                       (if (null? (car l)) (set-car! l (cons () ())) #f)
                                                                       (append-to-tail!
                                                                          (car l)
                                                                          (letrec ((__do_loop (lambda (j a)
                                                                                                (if (= j 0) a (__do_loop (- j 1) (cons () a))))))
                                                                             (__do_loop m ())))
                                                                       (__do_loop (cdr l)))))))
                                              (__do_loop l))
                                           (letrec ((__do_loop (lambda (l1 l2)
                                                                 (if (null? l2)
                                                                    #f
                                                                    (begin
                                                                       (set-cdr!
                                                                          (letrec ((__do_loop (lambda (j a)
                                                                                                (if (zero? j)
                                                                                                   a
                                                                                                   (begin
                                                                                                      (set-car! a i)
                                                                                                      (__do_loop (- j 1) (cdr a)))))))
                                                                             (__do_loop (quotient (length (car l2)) 2) (car l2)))
                                                                          (let ((n (quotient (length (car l1)) 2)))
                                                                             (if (= n 0)
                                                                                (begin
                                                                                   (set-car! l1 ())
                                                                                   (car l1))
                                                                                (letrec ((__do_loop (lambda (j a)
                                                                                                      (if (= j 1)
                                                                                                         (let ((x (cdr a)))
                                                                                                            (set-cdr! a ())
                                                                                                            x)
                                                                                                         (begin
                                                                                                            (set-car! a i)
                                                                                                            (__do_loop (- j 1) (cdr a)))))))
                                                                                   (__do_loop n (car l1))))))
                                                                       (__do_loop (cdr l1) (cdr l2)))))))
                                              (__do_loop l (cdr l))))
                                        (__do_loop (- i 1)))))))
               (__do_loop n))))
      (lambda (_n0 _m0)
         (let ((_l0 (letrec ((___do_loop0 (lambda (_i0 _a0)
                                           (if (= _i0 0)
                                              _a0
                                              (___do_loop0 (- _i0 1) (cons () _a0))))))
                      (___do_loop0 10 ()))))
            (letrec ((___do_loop1 (lambda (_i1)
                                    (if (= _i1 0)
                                       _l0
                                       (begin
                                          (if (null? (car _l0))
                                             (letrec ((___do_loop2 (lambda (_l1)
                                                                     (if (null? _l1)
                                                                        #f
                                                                        (begin
                                                                           (if (null? (car _l1))
                                                                              (set-car! _l1 (cons () ()))
                                                                              #f)
                                                                           (append-to-tail!
                                                                              (car _l1)
                                                                              (letrec ((___do_loop3 (lambda (_j0 _a1)
                                                                                                      (if (= _j0 0)
                                                                                                         _a1
                                                                                                         (___do_loop3 (- _j0 1) (cons () _a1))))))
                                                                                 (___do_loop3 _m0 ())))
                                                                           (___do_loop2 (cdr _l1)))))))
                                                (___do_loop2 _l0))
                                             (letrec ((___do_loop4 (lambda (_l10 _l20)
                                                                     (if (null? _l20)
                                                                        #f
                                                                        (begin
                                                                           (set-cdr!
                                                                              (letrec ((___do_loop5 (lambda (_j1 _a2)
                                                                                                      (if (zero? _j1)
                                                                                                         _a2
                                                                                                         (begin
                                                                                                            (set-car! _a2 _i1)
                                                                                                            (___do_loop5 (- _j1 1) (cdr _a2)))))))
                                                                                 (___do_loop5 (quotient (length (car _l20)) 2) (car _l20)))
                                                                              (let ((_n1 (quotient (length (car _l10)) 2)))
                                                                                 (if (= _n1 0)
                                                                                    (begin
                                                                                       (set-car! _l10 ())
                                                                                       (car _l10))
                                                                                    (letrec ((___do_loop6 (lambda (_j2 _a3)
                                                                                                            (if (= _j2 1)
                                                                                                               (let ((_x0 (cdr _a3)))
                                                                                                                  (set-cdr! _a3 ())
                                                                                                                  _x0)
                                                                                                               (begin
                                                                                                                  (set-car! _a3 _i1)
                                                                                                                  (___do_loop6 (- _j2 1) (cdr _a3)))))))
                                                                                       (___do_loop6 _n1 (car _l10))))))
                                                                           (___do_loop4 (cdr _l10) (cdr _l20)))))))
                                                (___do_loop4 _l0 (cdr _l0))))
                                          (___do_loop1 (- _i1 1)))))))
               (___do_loop1 _n0))))))
 
(equal?
   (destructive 600 50)
   (__toplevel_cons
      (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))
      (__toplevel_cons
         (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))
         (__toplevel_cons
            (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ()))))
            (__toplevel_cons
               (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ()))))
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 2 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons
                                             1
                                             (__toplevel_cons
                                                1
                                                (__toplevel_cons
                                                   1
                                                   (__toplevel_cons
                                                      1
                                                      (__toplevel_cons
                                                         1
                                                         (__toplevel_cons
                                                            1
                                                            (__toplevel_cons
                                                               1
                                                               (__toplevel_cons
                                                                  1
                                                                  (__toplevel_cons
                                                                     1
                                                                     (__toplevel_cons
                                                                        1
                                                                        (__toplevel_cons
                                                                           1
                                                                           (__toplevel_cons
                                                                              1
                                                                              (__toplevel_cons
                                                                                 2
                                                                                 (__toplevel_cons
                                                                                    2
                                                                                    (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 3 ())))))))))))))))))))))
                                 ())))))))))))
 
