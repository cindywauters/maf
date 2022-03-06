;; renamed lambdas/lets: 6
;; Of which consistent renamings: 4

(define permutations (lambda (x)
      (let ((x x)
            (perms (list x)))
         (define P (lambda (n)
               (if (> n 1)
                  (<change>
                     (letrec ((__do_loop (lambda (j)
                                           (if (zero? j)
                                              (P (- n 1))
                                              (begin
                                                 (P (- n 1))
                                                 (F n)
                                                 (__do_loop (- j 1)))))))
                        (__do_loop (- n 1)))
                     (letrec ((___do_loop0 (lambda (_j0)
                                             (if  (zero? _j0)
                                                (P (- n 1))
                                                (begin
                                                   (P (- n 2)) ;; NOT RENAMING changed 1 to 2
                                                   (F n)
                                                   (___do_loop0 (- _j0 1)))))))
                        (___do_loop0 (- n 1))))
                  #f)))
         (define F (<change>
               (lambda (n)
                  (set! x (revloop x n (list-tail x n)))
                  (set! perms (cons x perms)))
               (lambda (_n0)
                  (set! _n0 (revloop x _n0 (list-tail x _n0))) ;; NOT RENAMING, changed variable that is being set
                  (set! perms (cons x perms)))))
         (define revloop (<change>
               (lambda (x n y)
                  (if (zero? n)
                     y
                     (revloop (cdr x) (- n 1) (cons (car x) y))))
               (lambda (_x0 _n0 _y0)
                  (if (zero? _n0)
                     _y0
                     (revloop (cdr _x0) (- _n0 1) (cons (car _x0) _y0))))))
         (define list-tail (<change>
               (lambda (x n)
                  (if (zero? n) x (list-tail (cdr x) (- n 1))))
               (lambda (_x0 _n0)
                  (if (zero? _n0)
                     _x0
                     (list-tail (cdr _x0) (- _n0 1))))))
         (P (length x))
         perms)))
 
(define sumlists (lambda (x)
      (<change>
         (letrec ((__do_loop (lambda (x sum)
                               (if (null? x)
                                  sum
                                  (__do_loop
                                     (cdr x)
                                     (letrec ((__do_loop (lambda (y sum)
                                                           (if (null? y)
                                                              sum
                                                              (__do_loop (cdr y) (+ sum (car y)))))))
                                        (__do_loop (car x) sum)))))))
            (__do_loop x 0))
         (letrec ((___do_loop0 (lambda (_x0 _sum0)
                                 (if (null? _x0)
                                    _sum0
                                    (___do_loop0
                                       (cdr _x0)
                                       (letrec ((___do_loop1 (lambda (_y0 _sum1)
                                                               (if (null? _y0)
                                                                  _sum1
                                                                  (___do_loop1 (cdr _y0) (+ _sum1 (car _y0)))))))
                                          (___do_loop1 (car _x0) _sum0)))))))
            (___do_loop0 x 0)))))
 
(define one..n (lambda (n)
      (<change>
         (letrec ((__do_loop (lambda (n p)
                               (if (zero? n) p (__do_loop (- n 1) (cons n p))))))
            (__do_loop n ()))
         (letrec ((___do_loop0 (lambda (_n0 _p0)
                                 (if (zero? _n0)
                                    _p0
                                    (___do_loop0 (- _n0 1) (cons _n0 _p0))))))
            (___do_loop0 n ())))))
 
(define factorial (lambda (n)
      (if (= n 0) 1 (* n (factorial (- n 1))))))
 
(= (sumlists (permutations (one..n 9))) (* (quotient (* 9 (+ 9 1)) 2) (factorial 9)))
 
