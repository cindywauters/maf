;; renamed lambdas/lets: 4
 
(define permutations (lambda (x)
      (<change>
         (let ((x x)
               (perms (list x)))
            (define P (lambda (n)
                  (if (> n 1)
                     (letrec ((__do_loop (lambda (j)
                                           (if (zero? j)
                                              (P (- n 1))
                                              (begin
                                                 (P (- n 1))
                                                 (F n)
                                                 (__do_loop (- j 1)))))))
                        (__do_loop (- n 1)))
                     #f)))
            (define F (lambda (n)
                  (set! x (revloop x n (list-tail x n)))
                  (set! perms (cons x perms))))
            (define revloop (lambda (x n y)
                  (if (zero? n)
                     y
                     (revloop (cdr x) (- n 1) (cons (car x) y)))))
            (define list-tail (lambda (x n)
                  (if (zero? n) x (list-tail (cdr x) (- n 1)))))
            (P (length x))
            perms)
         (let ((_x0 x)
               (_perms0 (list x)))
            (define P (lambda (_n0)
                  (if (> _n0 1)
                     (letrec ((___do_loop0 (lambda (_j0)
                                             (if (zero? _j0)
                                                (P (- _n0 1))
                                                (begin
                                                   (P (- _n0 1))
                                                   (F _n0)
                                                   (___do_loop0 (- _j0 1)))))))
                        (___do_loop0 (- _n0 1)))
                     #f)))
            (define F (lambda (_n1)
                  (set! _x0 (revloop _x0 _n1 (list-tail _x0 _n1)))
                  (set! _perms0 (cons _x0 _perms0))))
            (define revloop (lambda (_x1 _n2 _y0)
                  (if (zero? _n2)
                     _y0
                     (revloop (cdr _x1) (- _n2 1) (cons (car _x1) _y0)))))
            (define list-tail (lambda (_x2 _n3)
                  (if (zero? _n3)
                     _x2
                     (list-tail (cdr _x2) (- _n3 1)))))
            (P (length _x0))
            _perms0))))
 
(define sumlists (lambda (x)
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
         (__do_loop x 0))))
 
(define one..n (lambda (n)
      (letrec ((__do_loop (<change>
                            (lambda (n p)
                               (if (zero? n) p (__do_loop (- n 1) (cons n p))))
                            (lambda (_n0 _p0)
                               (if (zero? _n0)
                                  _p0
                                  (__do_loop (- _n0 1) (cons _n0 _p0)))))))
         (__do_loop n ()))))
 
(define factorial (<change>
      (lambda (n)
         (if (= n 0) 1 (* n (factorial (- n 1)))))
      (lambda (_n0)
         (if (= _n0 0) 1 (* _n0 (factorial (- _n0 1)))))))
 
(= (sumlists (permutations (one..n 9))) (* (quotient (* 9 (+ 9 1)) 2) (factorial 9)))
 
