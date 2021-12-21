;; renamed lambdas/lets: 1
 
(define interval-list (<change>
      (lambda (m n)
         (if (> m n)
            ()
            (cons m (interval-list (+ 1 m) n))))
      (lambda (_m0 _n0)
         (if (> _m0 _n0)
            ()
            (cons _m0 (interval-list (+ 1 _m0) _n0))))))
 
(define sieve (lambda (l)
      (letrec ((remove-multiples (lambda (n l)
                                   (if (null? l)
                                      ()
                                      (if (= (modulo (car l) n) 0)
                                         (remove-multiples n (cdr l))
                                         (cons (car l) (remove-multiples n (cdr l))))))))
         (if (null? l)
            ()
            (cons (car l) (sieve (remove-multiples (car l) (cdr l))))))))
 
(define primes<= (lambda (n)
      (sieve (interval-list 2 n))))
 
(equal?
   (primes<= 100)
   (__toplevel_cons
      2
      (__toplevel_cons
         3
         (__toplevel_cons
            5
            (__toplevel_cons
               7
               (__toplevel_cons
                  11
                  (__toplevel_cons
                     13
                     (__toplevel_cons
                        17
                        (__toplevel_cons
                           19
                           (__toplevel_cons
                              23
                              (__toplevel_cons
                                 29
                                 (__toplevel_cons
                                    31
                                    (__toplevel_cons
                                       37
                                       (__toplevel_cons
                                          41
                                          (__toplevel_cons
                                             43
                                             (__toplevel_cons
                                                47
                                                (__toplevel_cons
                                                   53
                                                   (__toplevel_cons
                                                      59
                                                      (__toplevel_cons
                                                         61
                                                         (__toplevel_cons
                                                            67
                                                            (__toplevel_cons
                                                               71
                                                               (__toplevel_cons
                                                                  73
                                                                  (__toplevel_cons 79 (__toplevel_cons 83 (__toplevel_cons 89 (__toplevel_cons 97 ()))))))))))))))))))))))))))
 
