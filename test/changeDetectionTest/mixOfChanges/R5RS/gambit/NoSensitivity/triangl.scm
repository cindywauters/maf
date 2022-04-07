;; renamed lambdas/lets: 1
 
(define *board* (list->vector
      (cons
         1
         (cons
            1
            (cons
               1
               (cons
                  1
                  (cons
                     1
                     (cons
                        0
                        (cons
                           1
                           (cons
                              1
                              (cons
                                 1
                                 (cons
                                    1
                                    (cons
                                       1
                                       (cons
                                          1
                                          (cons 1 (cons 1 (cons 1 (cons 1 ()))))))))))))))))))
 
(define *sequence* (list->vector
      (cons
         0
         (cons
            0
            (cons
               0
               (cons
                  0
                  (cons
                     0
                     (cons
                        0
                        (cons
                           0
                           (cons
                              0
                              (cons
                                 0
                                 (cons
                                    0
                                    (cons 0 (cons 0 (cons 0 (cons 0 ()))))))))))))))))
 
(define *a* (list->vector
      (cons
         1
         (cons
            2
            (cons
               4
               (cons
                  3
                  (cons
                     5
                     (cons
                        6
                        (cons
                           1
                           (cons
                              3
                              (cons
                                 6
                                 (cons
                                    2
                                    (cons
                                       5
                                       (cons
                                          4
                                          (cons
                                             11
                                             (cons
                                                12
                                                (cons
                                                   13
                                                   (cons
                                                      7
                                                      (cons
                                                         8
                                                         (cons
                                                            4
                                                            (cons
                                                               4
                                                               (cons
                                                                  7
                                                                  (cons
                                                                     11
                                                                     (cons
                                                                        8
                                                                        (cons
                                                                           12
                                                                           (cons
                                                                              13
                                                                              (cons
                                                                                 6
                                                                                 (cons
                                                                                    10
                                                                                    (cons
                                                                                       15
                                                                                       (cons
                                                                                          9
                                                                                          (cons
                                                                                             14
                                                                                             (cons
                                                                                                13
                                                                                                (cons
                                                                                                   13
                                                                                                   (cons
                                                                                                      14
                                                                                                      (cons
                                                                                                         15
                                                                                                         (cons 9 (cons 10 (cons 6 (cons 6 ())))))))))))))))))))))))))))))))))))))))
 
(define *b* (list->vector
      (cons
         2
         (cons
            4
            (cons
               7
               (cons
                  5
                  (cons
                     8
                     (cons
                        9
                        (cons
                           3
                           (cons
                              6
                              (cons
                                 10
                                 (cons
                                    5
                                    (cons
                                       9
                                       (cons
                                          8
                                          (cons
                                             12
                                             (cons
                                                13
                                                (cons
                                                   14
                                                   (cons
                                                      8
                                                      (cons
                                                         9
                                                         (cons
                                                            5
                                                            (cons
                                                               2
                                                               (cons
                                                                  4
                                                                  (cons
                                                                     7
                                                                     (cons
                                                                        5
                                                                        (cons
                                                                           8
                                                                           (cons
                                                                              9
                                                                              (cons
                                                                                 3
                                                                                 (cons
                                                                                    6
                                                                                    (cons
                                                                                       10
                                                                                       (cons
                                                                                          5
                                                                                          (cons
                                                                                             9
                                                                                             (cons
                                                                                                8
                                                                                                (cons
                                                                                                   12
                                                                                                   (cons
                                                                                                      13
                                                                                                      (cons
                                                                                                         14
                                                                                                         (cons 8 (cons 9 (cons 5 (cons 5 ())))))))))))))))))))))))))))))))))))))))
 
(define *c* (list->vector
      (cons
         4
         (cons
            7
            (cons
               11
               (cons
                  8
                  (cons
                     12
                     (cons
                        13
                        (cons
                           6
                           (cons
                              10
                              (cons
                                 15
                                 (cons
                                    9
                                    (cons
                                       14
                                       (cons
                                          13
                                          (cons
                                             13
                                             (cons
                                                14
                                                (cons
                                                   15
                                                   (cons
                                                      9
                                                      (cons
                                                         10
                                                         (cons
                                                            6
                                                            (cons
                                                               1
                                                               (cons
                                                                  2
                                                                  (cons
                                                                     4
                                                                     (cons
                                                                        3
                                                                        (cons
                                                                           5
                                                                           (cons
                                                                              6
                                                                              (cons
                                                                                 1
                                                                                 (cons
                                                                                    3
                                                                                    (cons
                                                                                       6
                                                                                       (cons
                                                                                          2
                                                                                          (cons
                                                                                             5
                                                                                             (cons
                                                                                                4
                                                                                                (cons
                                                                                                   11
                                                                                                   (cons
                                                                                                      12
                                                                                                      (cons
                                                                                                         13
                                                                                                         (cons 7 (cons 8 (cons 4 (cons 4 ())))))))))))))))))))))))))))))))))))))))
 
(define *answer* ())
 
(define attempt (<change>
      (lambda (i depth)
         (if (= depth 14)
            (begin
               (set! *answer* (cons (cdr (vector->list *sequence*)) *answer*))
               #t)
            (if (if (= 1 (vector-ref *board* (vector-ref *a* i))) (if (= 1 (vector-ref *board* (vector-ref *b* i))) (= 0 (vector-ref *board* (vector-ref *c* i))) #f) #f)
               (begin
                  (vector-set! *board* (vector-ref *a* i) 0)
                  (vector-set! *board* (vector-ref *b* i) 0)
                  (vector-set! *board* (vector-ref *c* i) 1)
                  (vector-set! *sequence* depth i)
                  (letrec ((__do_loop (lambda (j depth)
                                        (if (let ((__or_res (= j 36))) (if __or_res __or_res (attempt j depth)))
                                           #f
                                           (__do_loop (+ j 1) depth)))))
                     (__do_loop 0 (+ depth 1)))
                  (vector-set! *board* (vector-ref *a* i) 1)
                  (vector-set! *board* (vector-ref *b* i) 1)
                  (vector-set! *board* (vector-ref *c* i) 0)
                  #f)
               #f)))
      (lambda (_i0 _depth0)
         (if (= _depth0 14)
            (begin
               (set! *answer* (cons (cdr (vector->list *sequence*)) *answer*))
               #t)
            (if (if (= 1 (vector-ref *board* (vector-ref *a* _i0))) (if (= 1 (vector-ref *board* (vector-ref *b* _i0))) (= 0 (vector-ref *board* (vector-ref *c* _i0))) #f) #f)
               (begin
                  (vector-set! *board* (vector-ref *a* _i0) 0)
                  (vector-set! *board* (vector-ref *b* _i0) 0)
                  (vector-set! *board* (vector-ref *c* _i0) 1)
                  (vector-set! *sequence* _depth0 _i0)
                  (letrec ((___do_loop0 (lambda (_j0 _depth1)
                                          (if (let ((___or_res0 (= _j0 36))) (if ___or_res0 ___or_res0 (attempt _j0 _depth1)))
                                             #f
                                             (___do_loop0 (+ _j0 1) _depth1)))))
                     (___do_loop0 0 (+ _depth0 1)))
                  (vector-set! *board* (vector-ref *a* _i0) 1)
                  (vector-set! *board* (vector-ref *b* _i0) 1)
                  (vector-set! *board* (vector-ref *c* _i0) 0)
                  #f)
               #f)))))
 
(define test (lambda (i depth)
      (set! *answer* ())
      (attempt i depth)
      (car *answer*)))
 
(equal?
   (test 22 1)
   (cons
      22
      (cons
         34
         (cons
            31
            (cons
               15
               (cons
                  7
                  (cons
                     1
                     (cons
                        20
                        (cons
                           17
                           (cons
                              25
                              (cons 6 (cons 5 (cons 13 (cons 32 ()))))))))))))))
 
