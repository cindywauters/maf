; Changes:
; * removed: 0
; * added: 1
; * swaps: 3
; * negated predicates: 1
(letrec ((append-to-tail! (lambda (x y)
                            (if (null? x)
                               y
                               ((letrec ((loop (lambda (a b)
                                                (if (null? b)
                                                   (begin
                                                      (set-cdr! a y)
                                                      x)
                                                   (loop b (cdr b))))))
                                  loop)
                                  x
                                  (cdr x)))))
         (destructive (lambda (n m)
                        (let ((l (letrec ((__do_loop (lambda (i a)
                                                      (if (= i 0) a (__do_loop (- i 1) (cons () a))))))
                                   (__do_loop 10 ()))))
                           (letrec ((__do_loop (lambda (i)
                                                 (if (= i 0)
                                                    l
                                                    (begin
                                                       (if (<change> (null? (car l)) (not (null? (car l))))
                                                          (letrec ((__do_loop (lambda (l)
                                                                                (if (null? l)
                                                                                   #f
                                                                                   (begin
                                                                                      (if (null? (car l)) (set-car! l (cons () ())) #f)
                                                                                      (<change>
                                                                                         (append-to-tail!
                                                                                            (car l)
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  (if (= j 0) a (__do_loop (- j 1) (cons () a))))))
                                                                                               (__do_loop m ())))
                                                                                         (__do_loop (cdr l)))
                                                                                      (<change>
                                                                                         (__do_loop (cdr l))
                                                                                         (append-to-tail!
                                                                                            (car l)
                                                                                            (letrec ((__do_loop (lambda (j a)
                                                                                                                  (if (= j 0) a (__do_loop (- j 1) (cons () a))))))
                                                                                               (__do_loop m ())))))))))
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
                                                                                                                           (<change>
                                                                                                                              (set-cdr! a ())
                                                                                                                              x)
                                                                                                                           (<change>
                                                                                                                              x
                                                                                                                              (set-cdr! a ())))
                                                                                                                        (begin
                                                                                                                           (<change>
                                                                                                                              (set-car! a i)
                                                                                                                              (__do_loop (- j 1) (cdr a)))
                                                                                                                           (<change>
                                                                                                                              (__do_loop (- j 1) (cdr a))
                                                                                                                              (set-car! a i)))))))
                                                                                                  (__do_loop n (car l1))))))
                                                                                      (__do_loop (cdr l1) (cdr l2)))))))
                                                             (__do_loop l (cdr l))))
                                                       (<change>
                                                          ()
                                                          1)
                                                       (__do_loop (- i 1)))))))
                              (__do_loop n))))))
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
                                    ()))))))))))))