;; renamed lambdas/lets: 1
 
(define square (lambda (x)
      (* x x)))
 
(define modulo-power (lambda (base exp n)
      (if (= exp 0)
         1
         (if (odd? exp)
            (modulo (* base (modulo-power base (- exp 1) n)) n)
            (modulo (square (modulo-power base (/ exp 2) n)) n)))))
 
(define is-trivial-composite? (<change>
      (lambda (n)
         (let ((__or_res (= (modulo n 2) 0)))
            (if __or_res
               __or_res
               (let ((__or_res (= (modulo n 3) 0)))
                  (if __or_res
                     __or_res
                     (let ((__or_res (= (modulo n 5) 0)))
                        (if __or_res
                           __or_res
                           (let ((__or_res (= (modulo n 7) 0)))
                              (if __or_res
                                 __or_res
                                 (let ((__or_res (= (modulo n 11) 0)))
                                    (if __or_res
                                       __or_res
                                       (let ((__or_res (= (modulo n 13) 0)))
                                          (if __or_res
                                             __or_res
                                             (let ((__or_res (= (modulo n 17) 0)))
                                                (if __or_res
                                                   __or_res
                                                   (let ((__or_res (= (modulo n 19) 0)))
                                                      (if __or_res __or_res (= (modulo n 23) 0))))))))))))))))))
      (lambda (_n0)
         (let ((___or_res0 (= (modulo _n0 2) 0)))
            (if ___or_res0
               ___or_res0
               (let ((___or_res1 (= (modulo _n0 3) 0)))
                  (if ___or_res1
                     ___or_res1
                     (let ((___or_res2 (= (modulo _n0 5) 0)))
                        (if ___or_res2
                           ___or_res2
                           (let ((___or_res3 (= (modulo _n0 7) 0)))
                              (if ___or_res3
                                 ___or_res3
                                 (let ((___or_res4 (= (modulo _n0 11) 0)))
                                    (if ___or_res4
                                       ___or_res4
                                       (let ((___or_res5 (= (modulo _n0 13) 0)))
                                          (if ___or_res5
                                             ___or_res5
                                             (let ((___or_res6 (= (modulo _n0 17) 0)))
                                                (if ___or_res6
                                                   ___or_res6
                                                   (let ((___or_res7 (= (modulo _n0 19) 0)))
                                                      (if ___or_res7 ___or_res7 (= (modulo _n0 23) 0))))))))))))))))))))
 
(define is-fermat-prime? (lambda (n iterations)
      (let ((__or_res (<= iterations 0)))
         (if __or_res
            __or_res
            (let* ((byte-size (ceiling (/ (log n) (log 2))))
                   (a (random byte-size)))
               (if (= (modulo-power a (- n 1) n) 1)
                  (is-fermat-prime? n (- iterations 1))
                  #f))))))
 
(define generate-fermat-prime (lambda (byte-size iterations)
      (let ((n (random byte-size)))
         (if (if (not (is-trivial-composite? n)) (is-fermat-prime? n iterations) #f)
            n
            (generate-fermat-prime byte-size iterations)))))
 
(define iterations 10)
 
(define byte-size 15)
 
(generate-fermat-prime byte-size iterations)
 
