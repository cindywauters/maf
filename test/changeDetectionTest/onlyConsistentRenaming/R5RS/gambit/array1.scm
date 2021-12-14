;; renamed lambdas/lets: 3
 
(define create-x (<change>
      (lambda (n)
         (define result (make-vector n 0))
         (letrec ((__do_loop (lambda (i)
                               (if (>= i n)
                                  result
                                  (begin
                                     (vector-set! result i i)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0)))
      (lambda (_n0)
         (define result (make-vector _n0 0))
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (>= _i0 _n0)
                                    result
                                    (begin
                                       (vector-set! result _i0 _i0)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(define create-y (<change>
      (lambda (x)
         (let* ((n (vector-length x))
                (result (make-vector n 0)))
            (letrec ((__do_loop (lambda (i)
                                  (if (< i 0)
                                     result
                                     (begin
                                        (vector-set! result i (vector-ref x i))
                                        (__do_loop (- i 1)))))))
               (__do_loop (- n 1)))))
      (lambda (_x0)
         (let* ((_n0 (vector-length _x0))
                (_result0 (make-vector _n0 0)))
            (letrec ((___do_loop0 (lambda (_i0)
                                    (if (< _i0 0)
                                       _result0
                                       (begin
                                          (vector-set! _result0 _i0 (vector-ref _x0 _i0))
                                          (___do_loop0 (- _i0 1)))))))
               (___do_loop0 (- _n0 1)))))))
 
(define my-try (lambda (n)
      (vector-length (create-y (create-x n)))))
 
(define go (<change>
      (lambda (n)
         ((letrec ((loop (lambda (repeat result)
                          (if (> repeat 0)
                             (loop (- repeat 1) (my-try n))
                             result))))
            loop)
            100
            ()))
      (lambda (_n0)
         ((letrec ((_loop0 (lambda (_repeat0 _result0)
                            (if (> _repeat0 0)
                               (_loop0 (- _repeat0 1) (my-try _n0))
                               _result0))))
            _loop0)
            100
            ()))))
 
(= 200 (go 200))
 
